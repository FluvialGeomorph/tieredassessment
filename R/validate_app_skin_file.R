#' Validate an Application Skin
#'
#' Load, merge, normalize, and validate an application skin using the same
#' startup path as [run_app()]. This provides downstream repositories and CI
#' workflows with a supported preflight check.
#'
#' @param skin_file Optional path to a downstream skin override. When omitted,
#'   normal override discovery is used.
#'
#' @return The normalized application skin, invisibly.
#' @export
validate_app_skin_file <- function(skin_file = NULL) {
  skin <- load_app_skin(override_file = skin_file)
  invisible(skin)
}
