#' Compose the compute registry
#'
#' Returns the registry (a nested list mapping `mode`/`fn` to handler
#' functions) used by the compute dispatcher. For now it returns only the
#' built-in registry; later we'll extend it to merge package/snippet registries.
#'
#' @return A nested named list of handler functions.
#' @examples
#' reg <- compose_registry()
#' is.list(reg$builtin)
#'
#' @export
compose_registry <- function() {
  # Later: merge builtin_registry with pkg/snippet registries, e.g.:
  # registry <- modifyList(builtin_registry, pkg_registry, keep.null = TRUE)
  # modifyList(registry, snippet_registry, keep.null = TRUE)
  builtin_registry
}
