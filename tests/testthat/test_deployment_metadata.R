test_that("deployment metadata excludes obsolete and circular dependencies", {
  project_file <- function(...) {
    testthat::test_path("..", "..", ...)
  }

  app <- readLines(project_file("app.R"), warn = FALSE)
  description <- readLines(project_file("DESCRIPTION"), warn = FALSE)
  lockfile <- readLines(project_file("renv.lock"), warn = FALSE)
  manifest <- readLines(project_file("manifest.json"), warn = FALSE)
  terra_source_sha <- "70ad1a4363888e78fa3ddb1a9a59f9dbf50cef18"
  aws_signature_source_sha <- "144af741c9e55badf3eafe18d184f8ecb717cb00"

  expect_true(any(grepl("run_app()", app, fixed = TRUE)))
  expect_false(any(grepl("ohwm2::run_app", app, fixed = TRUE)))
  expect_false(any(grepl('"ohwm2": {', lockfile, fixed = TRUE)))
  expect_false(any(grepl('"ohwm2": {', manifest, fixed = TRUE)))
  expect_false(any(grepl("shinyValidator", lockfile, fixed = TRUE)))
  expect_false(any(grepl("shinyValidator", manifest, fixed = TRUE)))
  expect_true(any(grepl('"platform": "4.6.0"', manifest, fixed = TRUE)))
  expect_true(any(grepl(
    paste0("rspatial/terra@", terra_source_sha),
    description,
    fixed = TRUE
  )))
  expect_true(any(grepl(terra_source_sha, lockfile, fixed = TRUE)))
  expect_true(any(grepl(terra_source_sha, manifest, fixed = TRUE)))
  expect_true(any(grepl(
    paste0("cloudyr/aws.signature@", aws_signature_source_sha),
    description,
    fixed = TRUE
  )))
  expect_true(any(grepl(aws_signature_source_sha, lockfile, fixed = TRUE)))
  expect_true(any(grepl(aws_signature_source_sha, manifest, fixed = TRUE)))
})
