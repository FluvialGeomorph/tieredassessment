#' Check a Downstream Application Repository
#'
#' Run the read-only downstream repository validation and print a concise
#' plain-language success summary. Validation errors include the failed
#' requirement and stop the check.
#'
#' @inheritParams validate_downstream_repository
#'
#' @return The same machine-readable evidence as
#'   [validate_downstream_repository()], invisibly.
#' @export
check_downstream_repository <- function(
  file = ".fluvial-app.yml",
  repository = "."
) {
  evidence <- validate_downstream_repository(
    file = file,
    repository = repository
  )

  customer_asset_count <- length(
    evidence$application$referenced_customer_assets
  )
  cat(
    paste0(
      "PASS: downstream repository preflight\n",
      "  Application: ", evidence$metadata$application_id, "\n",
      "  Customer repository: ", evidence$git$origin$repository, "\n",
      "  Upstream release: ", evidence$git$upstream$release, "\n",
      "  Downstream commit: ", evidence$git$head, "\n",
      "  Skin schema: ", evidence$application$schema_version, "\n",
      "  Referenced customer assets: ", customer_asset_count, "\n"
    )
  )

  invisible(evidence)
}
