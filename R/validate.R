#' Validate a compute request payload
#'
#' Minimal structural checks for the JSON body received by the compute API.
#' This is intentionally lightweight and will gain richer schema validation later.
#'
#' @param x A list representing the request body (already parsed from JSON).
#'   Must contain at least components `mode` and `fn`.
#'
#' @return Invisibly returns `TRUE` on success; otherwise throws an error.
#' @examples
#' validate_payload(list(mode = "builtin", fn = "score_move"))
#'
#' @export
validate_payload <- function(x) {
  stopifnot(is.list(x))
  req <- c("mode", "fn")
  miss <- req[!req %in% names(x)]
  if (length(miss)) stop("Missing: ", paste(miss, collapse = ", "))
  invisible(TRUE)
}
