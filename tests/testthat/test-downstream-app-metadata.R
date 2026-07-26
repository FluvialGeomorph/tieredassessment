valid_downstream_metadata <- function() {
  list(
    schema_version = 1,
    application_id = "floodplain-connectivity",
    upstream = list(
      repository = "https://github.com/FluvialGeomorph/ohwm2.git",
      remote = "upstream",
      release = "2026.07.25"
    ),
    skin = list(
      file = "inst/app/skin.yml",
      schema_version = 1,
      customer_assets = "inst/app/www/customer"
    ),
    deployment = list(
      manifest = "manifest.json",
      dependency_resolution = "library"
    )
  )
}

write_downstream_metadata <- function(metadata) {
  file <- tempfile(fileext = ".yml")
  yaml::write_yaml(metadata, file)
  file
}

test_that("version 1 downstream metadata is accepted", {
  metadata <- valid_downstream_metadata()

  expect_identical(
    validate_downstream_app_metadata(metadata),
    metadata
  )
})

test_that("exported validator reads and returns downstream metadata", {
  file <- write_downstream_metadata(valid_downstream_metadata())
  withr::defer(unlink(file))

  metadata <- validate_downstream_app_file(file)

  expect_equal(metadata$application_id, "floodplain-connectivity")
  expect_equal(metadata$upstream$release, "2026.07.25")
})

test_that("downstream metadata file must exist and contain a mapping", {
  missing_file <- tempfile(fileext = ".yml")

  expect_error(
    validate_downstream_app_file(missing_file),
    "does not exist"
  )

  empty_file <- tempfile(fileext = ".yml")
  withr::defer(unlink(empty_file))
  writeLines(character(), empty_file)

  expect_error(
    validate_downstream_app_file(empty_file),
    "is empty"
  )

  sequence_file <- tempfile(fileext = ".yml")
  withr::defer(unlink(sequence_file))
  writeLines(c("- first", "- second"), sequence_file)

  expect_error(
    validate_downstream_app_file(sequence_file),
    "must be a named mapping"
  )
})

test_that("downstream metadata rejects missing and unknown fields", {
  missing <- valid_downstream_metadata()
  missing$upstream$release <- NULL
  expect_error(
    validate_downstream_app_metadata(missing),
    "missing required field.*release"
  )

  unknown <- valid_downstream_metadata()
  unknown$unrecognized <- TRUE
  expect_error(
    validate_downstream_app_metadata(unknown),
    "unknown field.*unrecognized"
  )
})

test_that("downstream metadata rejects invalid identifiers and versions", {
  invalid_id <- valid_downstream_metadata()
  invalid_id$application_id <- "Floodplain Connectivity"
  expect_error(
    validate_downstream_app_metadata(invalid_id),
    "must match"
  )

  unsupported_metadata <- valid_downstream_metadata()
  unsupported_metadata$schema_version <- 2
  expect_error(
    validate_downstream_app_metadata(unsupported_metadata),
    "metadata.schema_version.*supported value 1"
  )

  unsupported_skin <- valid_downstream_metadata()
  unsupported_skin$skin$schema_version <- 2
  expect_error(
    validate_downstream_app_metadata(unsupported_skin),
    "metadata.skin.schema_version.*supported value 1"
  )
})

test_that("downstream metadata enforces canonical upstream settings", {
  wrong_repository <- valid_downstream_metadata()
  wrong_repository$upstream$repository <- "https://example.com/ohwm2.git"
  expect_error(
    validate_downstream_app_metadata(wrong_repository),
    "metadata.upstream.repository"
  )

  wrong_remote <- valid_downstream_metadata()
  wrong_remote$upstream$remote <- "origin"
  expect_error(
    validate_downstream_app_metadata(wrong_remote),
    "metadata.upstream.remote"
  )
})

test_that("downstream metadata enforces owned paths and manifest mode", {
  wrong_skin <- valid_downstream_metadata()
  wrong_skin$skin$file <- "skin.yml"
  expect_error(
    validate_downstream_app_metadata(wrong_skin),
    "metadata.skin.file"
  )

  wrong_assets <- valid_downstream_metadata()
  wrong_assets$skin$customer_assets <- "www/customer"
  expect_error(
    validate_downstream_app_metadata(wrong_assets),
    "metadata.skin.customer_assets"
  )

  wrong_manifest <- valid_downstream_metadata()
  wrong_manifest$deployment$manifest <- "connect/manifest.json"
  expect_error(
    validate_downstream_app_metadata(wrong_manifest),
    "metadata.deployment.manifest"
  )

  wrong_resolution <- valid_downstream_metadata()
  wrong_resolution$deployment$dependency_resolution <- "renv"
  expect_error(
    validate_downstream_app_metadata(wrong_resolution),
    "metadata.deployment.dependency_resolution"
  )
})
