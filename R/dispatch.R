#' Fallback operator
#'
#' Returns `b` when `a` is `NULL`, otherwise returns `a`.
#'
#' @name fallback
#' @aliases `%||%`
#' @usage a \%||\% b
#' @param a,b Any R objects.
#' @return One of `a` or `b`.
#' @keywords internal
#' @export
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Dispatch a compute request
#'
#' Routes a request to an allow-listed handler function based on `mode` and `fn`.
#' The dispatcher is responsible for (optional) RNG seeding and calling the
#' appropriate handler in `registry`.
#'
#' @param mode Character scalar identifying a registry "namespace"
#'   (e.g., `"builtin"`, `"pkg"`, `"snippet"`).
#' @param fn Character scalar naming the allowed function within `mode`.
#' @param args A list of arguments passed to the handler (default: empty list).
#' @param seed Optional integer. When supplied, the RNG is seeded for
#'   deterministic computations.
#' @param registry A nested named list of handlers. Expected shape:
#'   `registry[[mode]][[fn]]` is a function that accepts a single list `args`.
#'
#' @return The handler's return value (typically a list).
#'
#' @examples
#' reg <- list(builtin = list(add1 = function(args) list(x = (args$x %||% 0) + 1L)))
#' dispatch("builtin", "add1", list(x = 2L), registry = reg)
#'
#' @export
dispatch <- function(mode, fn, args = list(), seed = NULL, registry = list()) {
  if (!is.null(seed)) { set.seed(as.integer(seed)); RNGkind("Mersenne-Twister") }
  reg_mode <- registry[[mode]]
  if (is.null(reg_mode)) stop("Unknown mode: ", mode)
  h <- reg_mode[[fn]]
  if (is.null(h)) stop("Unknown function: ", fn, " for mode: ", mode)
  h(args %||% list())
}
