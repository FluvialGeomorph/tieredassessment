#' Read downstream application metadata
#'
#' @param file Path to a downstream application metadata file.
#'
#' @return The parsed metadata as a list.
#' @noRd
read_downstream_app_file <- function(file) {
  assert_downstream_text(file, "file")
  if (!file.exists(file)) {
    stop("Downstream application metadata file does not exist: ", file,
         call. = FALSE)
  }

  metadata <- yaml::read_yaml(file, eval.expr = FALSE)
  if (is.null(metadata)) {
    stop("Downstream application metadata file is empty: ", file,
         call. = FALSE)
  }

  metadata
}

#' Validate downstream application metadata
#'
#' @param metadata Parsed downstream application metadata.
#'
#' @return The validated metadata, invisibly.
#' @noRd
validate_downstream_app_metadata <- function(metadata) {
  assert_downstream_fields(
    metadata,
    path = "metadata",
    required = c(
      "schema_version",
      "application_id",
      "upstream",
      "skin",
      "deployment"
    )
  )
  assert_downstream_version(
    metadata$schema_version,
    "metadata.schema_version"
  )

  assert_downstream_text(
    metadata$application_id,
    "metadata.application_id"
  )
  if (!grepl("^[a-z][a-z0-9-]*$", metadata$application_id)) {
    stop(
      "`metadata.application_id` must match `^[a-z][a-z0-9-]*$`.",
      call. = FALSE
    )
  }

  assert_downstream_fields(
    metadata$upstream,
    path = "metadata.upstream",
    required = c("repository", "remote", "release")
  )
  assert_downstream_exact(
    metadata$upstream$repository,
    "metadata.upstream.repository",
    "https://github.com/FluvialGeomorph/ohwm2.git"
  )
  assert_downstream_exact(
    metadata$upstream$remote,
    "metadata.upstream.remote",
    "upstream"
  )
  assert_downstream_text(
    metadata$upstream$release,
    "metadata.upstream.release"
  )

  assert_downstream_fields(
    metadata$skin,
    path = "metadata.skin",
    required = c("file", "schema_version", "customer_assets")
  )
  assert_downstream_exact(
    metadata$skin$file,
    "metadata.skin.file",
    "inst/app/skin.yml"
  )
  assert_downstream_version(
    metadata$skin$schema_version,
    "metadata.skin.schema_version"
  )
  assert_downstream_exact(
    metadata$skin$customer_assets,
    "metadata.skin.customer_assets",
    "inst/app/www/customer"
  )

  assert_downstream_fields(
    metadata$deployment,
    path = "metadata.deployment",
    required = c("manifest", "dependency_resolution")
  )
  assert_downstream_exact(
    metadata$deployment$manifest,
    "metadata.deployment.manifest",
    "manifest.json"
  )
  assert_downstream_exact(
    metadata$deployment$dependency_resolution,
    "metadata.deployment.dependency_resolution",
    "library"
  )

  invisible(metadata)
}

#' @noRd
assert_downstream_fields <- function(value, path, required) {
  if (!is.list(value) || is.null(names(value)) || any(!nzchar(names(value)))) {
    stop("`", path, "` must be a named mapping.", call. = FALSE)
  }

  missing <- setdiff(required, names(value))
  if (length(missing) > 0L) {
    stop(
      "`", path, "` is missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  unknown <- setdiff(names(value), required)
  if (length(unknown) > 0L) {
    stop(
      "`", path, "` contains unknown field(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(value)
}

#' @noRd
assert_downstream_text <- function(value, path) {
  if (!is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)) {
    stop("`", path, "` must be one non-empty string.", call. = FALSE)
  }

  invisible(value)
}

#' @noRd
assert_downstream_version <- function(value, path) {
  if (!is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      value != 1) {
    stop("`", path, "` must be the supported value 1.", call. = FALSE)
  }

  invisible(value)
}

#' @noRd
assert_downstream_exact <- function(value, path, expected) {
  assert_downstream_text(value, path)
  if (!identical(value, expected)) {
    stop(
      "`", path, "` must be `", expected, "`.",
      call. = FALSE
    )
  }

  invisible(value)
}
