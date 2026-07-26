#' Validate a Downstream Application Repository
#'
#' Validate `.fluvial-app.yml` and inspect local Git evidence for a downstream
#' customer application. The repository must be clean, its configured `origin`
#' must be the protected default push destination, its configured `upstream`
#' remote must use the canonical fetch URL and a disabled push URL, the
#' recorded release tag must resolve locally and be an ancestor of `HEAD`, and
#' changes since that release must remain within downstream-owned paths. The
#' configured skin is merged with the repository's template defaults,
#' validated, and checked for tracked customer-asset references.
#'
#' This function is read-only. It does not fetch tags, modify Git state,
#' generate a manifest, inspect renv state, run tests, or deploy the
#' application.
#'
#' @param file Path to `.fluvial-app.yml`.
#' @param repository Path within the downstream Git repository.
#'
#' @return A machine-readable list of validated metadata and local Git
#'   evidence, invisibly.
#' @export
validate_downstream_repository <- function(
  file = ".fluvial-app.yml",
  repository = "."
) {
  metadata <- validate_downstream_app_file(file)
  git_state <- inspect_downstream_git_state(
    metadata,
    repository,
    metadata_file = file
  )
  app_state <- inspect_downstream_app_state(metadata, git_state)

  invisible(list(
    schema_version = 1L,
    metadata = metadata,
    git = git_state,
    application = app_state
  ))
}
