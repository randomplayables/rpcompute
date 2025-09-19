#' Internal: capture versions of loaded packages
#'
#' Returns a named list mapping package names to version strings for all
#' *currently loaded* namespaces. This is smaller and more relevant than
#' enumerating every installed package.
#'
#' @return A named list of character versions, one per loaded namespace.
#' @examples
#' # Internal helper; shown for clarity only
#' .package_versions()
#'
#' @keywords internal
#' @noRd
.package_versions <- function() {
  pkgs <- loadedNamespaces()
  stats::setNames(
    lapply(pkgs, function(p) as.character(utils::packageVersion(p))),
    pkgs
  )
}

#' Capture runtime provenance
#'
#' Returns a compact, extensible list describing the runtime environment.
#' This variant records the R version and **loaded** package versions
#' (rather than all installed packages) to keep response payloads small.
#'
#' @param extra A named list of additional fields to append (optional).
#'
#' @return A named list with elements:
#' \describe{
#'   \item{r}{R version as character.}
#'   \item{packages}{Named list of versions for loaded packages.}
#'   \item{...}{Any fields passed via `extra` (e.g., `latencyMs`).}
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
    packages = .package_versions()
  )
  c(base, extra)
}
