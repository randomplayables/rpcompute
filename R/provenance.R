#' Capture runtime provenance
#'
#' Returns a small, extensible list describing the runtime environment.
#' Used to attach reproducibility metadata (R version, package versions, etc.)
#' to each compute result.
#'
#' @param extra A named list of additional fields to append (optional).
#'
#' @return A named list with elements:
#' \describe{
#'   \item{r}{R version as character.}
#'   \item{packages}{Named list of installed package versions.}
#'   \item{...}{Any fields passed via `extra`.}
#' }
#'
#' @examples
#' p <- capture_provenance()
#' is.list(p)
#'
#' @export
capture_provenance <- function(extra = list()) {
  base <- list(
    r = as.character(getRversion()),
    packages = as.list(utils::installed.packages()[, "Version"])
  )
  c(base, extra)
}
