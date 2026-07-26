#' Validate Downstream Application Metadata
#'
#' Read and validate the versioned `.fluvial-app.yml` contract used by
#' downstream customer applications. This check validates only the metadata
#' structure and fixed version 1 conventions; it does not inspect Git history
#' or deployment state.
#'
#' @param file Path to the downstream application metadata file.
#'
#' @return The parsed and validated metadata, invisibly.
#' @export
validate_downstream_app_file <- function(file = ".fluvial-app.yml") {
  metadata <- read_downstream_app_file(file)
  validate_downstream_app_metadata(metadata)
  invisible(metadata)
}
