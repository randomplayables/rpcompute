#' Start the rpcompute plumber API
#'
#' Launches a small HTTP API that exposes a single `POST /compute` endpoint.
#' The endpoint validates the payload, dispatches to an allow-listed handler,
#' and returns the handler result plus provenance metadata.
#'
#' @param host Host interface to bind. Default `"0.0.0.0"`.
#' @param port Integer port to listen on. Default `8080`.
#' @param registry A registry object (nested list) mapping `mode`/`fn` to
#'   handler functions. Defaults to `builtin_registry`.
#'
#' @return (Invisibly) the running plumber router object.
#'
#' @examples
#' \dontrun{
#'   # Start a local server on port 8080
#'   serve(port = 8080)
#' }
#'
#' @export
serve <- function(host = "0.0.0.0", port = 8080, registry = compose_registry()) {

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
  pr <- plumber::pr_post(pr, "/compute", h)

  plumber::pr_run(pr, host = host, port = as.integer(port))
  invisible(pr)
}
