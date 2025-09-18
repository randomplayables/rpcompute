#' Built-in handler registry
#'
#' A minimal allow-listed registry of built-in compute handlers. This exists
#' so the service can start and be integration-tested before package/snippet
#' registries are wired up.
#'
#' @format A named list of lists. The shape is
#'   `builtin_registry[[mode]][[fn]] <- function(args) { ... }`.
#'
#' @examples
#' builtin_registry$builtin$score_move(list(state = list(x = 5L)))
#'
#' @export
builtin_registry <- list(
  builtin = list(
    score_move = function(args) {
      s <- args$state %||% list(x = 0L)
      list(score = as.integer(s$x) + 7L, meta = list(p = 0.03))
    }
  )
)
