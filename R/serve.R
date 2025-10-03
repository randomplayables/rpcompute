#' Start the rpcompute HTTP API
#'
#' Launch a small HTTP API that exposes:
#' \itemize{
#'   \item \code{GET /healthz} – liveness probe, returns \code{{"status":"ok"}}.
#'   \item \code{POST /compute} – validates the payload, dispatches to an
#'         allow-listed handler, and returns the handler result plus provenance.
#' }
#'
#' Requests that fail validation/dispatch return uniform JSON errors with
#' HTTP 4xx and shape \code{{"ok": false, "error": "<message>"}}.
#'
#' Optionally, a very light shared-secret check can be enabled. When enabled,
#' clients must send the header \code{X-RP-Secret: <value>} and the server will
#' compare it to the value stored in the environment variable named by
#' \code{secret_env}.
#'
#' @param host Host interface to bind. Default \code{"0.0.0.0"}.
#' @param port Integer port to listen on. Default \code{8080}.
#' @param registry A registry (nested list) mapping \code{mode}/\code{fn} to
#'   handler functions. Defaults to \code{compose_registry()}.
#' @param require_secret Logical. If \code{TRUE}, require the request header
#'   \code{X-RP-Secret} to match the configured secret; otherwise no header is
#'   required. You can also set the environment variable
#'   \code{RPCOMPUTE_REQUIRE_SECRET=true} to enable this without changing code.
#'   Default \code{FALSE}.
#' @param secret_env Character name of the environment variable that holds the
#'   shared secret used when \code{require_secret = TRUE}. Default
#'   \code{"RPCOMPUTE_SECRET"}.
#'
#' @return (Invisibly) the running \pkg{plumber} router object.
#'
#' @section CORS:
#' The service responds to browser preflight requests and sets CORS headers.
#' The allowed origin can be controlled by environment variable
#' \code{RPCOMPUTE_ALLOW_ORIGIN} (e.g., \code{"https://your-frontend.example"}).
#' Default is \code{"*"}.
#' The following headers are emitted:
#' \itemize{
#'   \item \code{Access-Control-Allow-Origin}: matched origin or \code{"*"}.
#'   \item \code{Access-Control-Allow-Methods}: \code{POST, GET, OPTIONS}.
#'   \item \code{Access-Control-Allow-Headers}: \code{Content-Type, X-RP-Secret}.
#'   \item \code{Access-Control-Max-Age}: \code{600}.
#'   \item \code{Vary}: \code{Origin}.
#' }
#'
#' @section Security:
#' The shared-secret check is a minimal protection suitable for server-to-server
#' calls behind a trusted gateway. For public internet exposure, consider
#' terminating auth at your gateway (e.g., Cloud Run IAM, OAuth/JWT) and keep
#' this service private.
#'
#' @examples
#' \dontrun{
#'   # Start a local server on port 8080
#'   serve(port = 8080)
#'
#'   # Start with a shared secret (read from env var RPCOMPUTE_SECRET)
#'   Sys.setenv(RPCOMPUTE_SECRET = "abc123")
#'   serve(port = 8080, require_secret = TRUE)
#'
#'   # If you keep secrets under a different env var name:
#'   Sys.setenv(MY_SECRET = "topsecret")
#'   serve(port = 8080, require_secret = TRUE, secret_env = "MY_SECRET")
#' }
#'
#' @export
serve <- function(
    host = "0.0.0.0",
    port = 8080,
    registry = compose_registry(),
    require_secret = identical(tolower(Sys.getenv("RPCOMPUTE_REQUIRE_SECRET", "false")), "true"),
    secret_env = "RPCOMPUTE_SECRET"
) {

  h <- function(req, res) {
    t0 <- proc.time()[["elapsed"]]
    tryCatch({
      body <- req$body
      validate_payload(body)
      out <- dispatch(body$mode %||% "builtin",
                      body$fn,
                      body$args %||% list(),
                      body$seed,
                      registry)
      ms <- as.integer((proc.time()[["elapsed"]] - t0) * 1000)
      list(ok = TRUE, result = out,
           provenance = capture_provenance(list(latencyMs = ms)))
    }, error = function(e) {
      res$status <- 400
      list(ok = FALSE, error = unname(conditionMessage(e)))
    })
  }

  pr <- plumber::pr()

  # --- request logging (one JSON line per request) --------------------------
  pr <- plumber::pr_filter(pr, "log", function(req, res) {
    started <- proc.time()[["elapsed"]]
    # Cloud Run / Google load balancers often pass trace or request IDs:
    rid <- req$HTTP_X_CLOUD_TRACE_CONTEXT %||%
      req$HTTP_X_REQUEST_ID %||%
      sprintf("%08x", as.integer(stats::runif(1, 0, 2^31 - 1)))

    # Run the downstream handler
    plumber::forward()

    # After handler finishes, compute latency and emit a compact JSON log
    on.exit({
      ms <- as.integer((proc.time()[["elapsed"]] - started) * 1000)
      status <- res$status %||% 200L
      # Keep the log small but useful; stdout goes to Cloud Logging.
      log <- list(
        ts      = as.integer(Sys.time()),
        method  = req$REQUEST_METHOD,
        path    = req$PATH_INFO,
        status  = status,
        ms      = ms,
        rid     = rid,
        ua      = req$HTTP_USER_AGENT %||% NULL,
        ip      = req$REMOTE_ADDR %||% NULL
      )
      cat(jsonlite::toJSON(log, auto_unbox = TRUE), "\n")
    }, add = TRUE)
  })

  # --- /version --------------------------------------------------------------
  pr <- plumber::pr_get(pr, "/version", function() {
    list(
      package = "rpcompute",
      version = as.character(utils::packageVersion("rpcompute")),
      git_sha = Sys.getenv("GIT_SHA", "")   # populated at build time (see below)
    )
  })

  # --- CORS filter (handles preflight and sets headers) ---
  pr <- plumber::pr_filter(pr, "cors", function(req, res) {
    raw <- Sys.getenv("RPCOMPUTE_ALLOW_ORIGIN", "*")
    origin <- req$HTTP_ORIGIN %||% ""

    allow <- function(origin, raw) {
      raw <- trimws(raw)
      if (raw == "*") return("*")

      # split comma-separated list
      parts <- strsplit(raw, ",", fixed = TRUE)[[1]]
      parts <- trimws(parts)

      # exact match
      if (nzchar(origin) && origin %in% parts) return(origin)

      # simple wildcard support: patterns like *.randomplayables.com
      for (p in parts) {
        if (startsWith(p, "*.")) {
          suffix <- substring(p, 2L) # like ".randomplayables.com"
          if (nzchar(origin) && endsWith(origin, suffix)) return(origin)
        }
      }
      return(NULL)
    }

    chosen <- allow(origin, raw)

    if (identical(raw, "*")) {
      res$setHeader("Access-Control-Allow-Origin", "*")
    } else if (!is.null(chosen)) {
      res$setHeader("Access-Control-Allow-Origin", chosen)
    } else {
      res$setHeader("Access-Control-Allow-Origin", "null") # not allowed
    }

    res$setHeader("Vary", "Origin")
    res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, X-RP-Secret")
    res$setHeader("Access-Control-Max-Age", "600")

    if (identical(req$REQUEST_METHOD, "OPTIONS")) {
      return("")  # 200 OK with headers
    }
    plumber::forward()
  })

  # Optional very-light auth via shared secret header
  if (isTRUE(require_secret)) {
    pr <- plumber::pr_filter(pr, "secret", function(req, res) {
      expected <- Sys.getenv(secret_env, "")
      got <- req$HTTP_X_RP_SECRET %||% ""
      if (!nzchar(expected) || !identical(got, expected)) {
        res$status <- 401
        return(list(ok = FALSE, error = "unauthorized"))
      }
      plumber::forward()
    })
  }

  # Health + main endpoint
  pr <- plumber::pr_get(pr,  "/healthz", function() list(status = "ok"))
  pr <- plumber::pr_post(pr, "/compute", h)

  plumber::pr_run(pr, host = host, port = as.integer(port))
  invisible(pr)
}
