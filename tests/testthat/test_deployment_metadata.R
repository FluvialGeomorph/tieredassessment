test_that("deployment metadata excludes obsolete and circular dependencies", {
  project_file <- function(...) {
    testthat::test_path("..", "..", ...)
  }

  app <- readLines(project_file("app.R"), warn = FALSE)
  lockfile <- readLines(project_file("renv.lock"), warn = FALSE)
  manifest <- readLines(project_file("manifest.json"), warn = FALSE)

  expect_true(any(grepl("run_app()", app, fixed = TRUE)))
  expect_false(any(grepl("ohwm2::run_app", app, fixed = TRUE)))
  expect_false(any(grepl('"ohwm2": {', lockfile, fixed = TRUE)))
  expect_false(any(grepl('"ohwm2": {', manifest, fixed = TRUE)))
  expect_false(any(grepl("shinyValidator", lockfile, fixed = TRUE)))
  expect_false(any(grepl("shinyValidator", manifest, fixed = TRUE)))
})
