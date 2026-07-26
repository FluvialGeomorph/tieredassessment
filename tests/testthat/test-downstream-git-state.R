run_test_git <- function(repository, arguments) {
  output <- suppressWarnings(
    system2(
      Sys.which("git"),
      c("-C", shQuote(repository), arguments),
      stdout = TRUE,
      stderr = TRUE
    )
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(paste(output, collapse = "\n"), call. = FALSE)
  }
  unname(output)
}

git_test_downstream_metadata <- function() {
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

create_downstream_repository <- function() {
  repository <- tempfile("downstream-repository-")
  dir.create(repository)
  run_test_git(repository, c("init", "--initial-branch=main"))
  run_test_git(repository, c("config", "user.name", "Test Maintainer"))
  run_test_git(repository, c("config", "user.email", "test@example.com"))
  run_test_git(repository, c("config", "commit.gpgsign", "false"))

  writeLines("shared upstream content", file.path(repository, "README.md"))
  dir.create(file.path(repository, "inst", "app", "www"), recursive = TRUE)
  writeLines(
    c(
      "default:",
      "  schema_version: 1",
      "  identity:",
      "    app_title: Upstream Test",
      "    browser_title: Upstream Test",
      "    favicon: www/favicon.png",
      "  theme:",
      "    bootswatch: sandstone",
      "    version: 5",
      "  workflow:",
      "    draw_xs:",
      "      nav_label: Draw XS",
      "      sidebar_title: Draw a cross section",
      "      instructions:",
      "        - Draw the cross section.",
      "      next_button: Draw Flowline",
      "      progress_message: Retrieving elevation data",
      "    draw_flowline:",
      "      nav_label: Draw Flowline",
      "      sidebar_title: Draw a flowline",
      "      instructions:",
      "        - Draw the flowline.",
      "      next_button: Review Results",
      "      progress_message: Calculating results",
      "    results:",
      "      nav_label: Results"
    ),
    file.path(repository, "inst", "app", "skin-default.yml")
  )
  writeBin(
    as.raw(c(137, 80, 78, 71)),
    file.path(repository, "inst", "app", "www", "favicon.png")
  )
  run_test_git(repository, c("add", "README.md", "inst/app"))
  run_test_git(repository, c("commit", "-m", shQuote("upstream release")))
  run_test_git(repository, c("tag", "2026.07.25"))
  run_test_git(
    repository,
    c(
      "remote",
      "add",
      "upstream",
      "https://github.com/FluvialGeomorph/ohwm2.git"
    )
  )
  run_test_git(
    repository,
    c("remote", "set-url", "--push", "upstream", "DISABLED")
  )
  run_test_git(
    repository,
    c(
      "remote",
      "add",
      "origin",
      "https://github.com/FluvialGeomorph/floodplain-connectivity.git"
    )
  )
  run_test_git(repository, c("config", "remote.pushDefault", "origin"))

  metadata <- git_test_downstream_metadata()
  yaml::write_yaml(metadata, file.path(repository, ".fluvial-app.yml"))
  writeLines(
    c(
      "default:",
      "  identity:",
      "    app_title: Test",
      "    browser_title: Test",
      "    favicon: www/customer/favicon.png"
    ),
    file.path(repository, "inst", "app", "skin.yml")
  )
  dir.create(file.path(repository, "inst", "app", "www", "customer"))
  writeBin(
    as.raw(c(137, 80, 78, 71)),
    file.path(repository, "inst", "app", "www", "customer", "favicon.png")
  )
  run_test_git(
    repository,
    c(
      "add",
      ".fluvial-app.yml",
      "inst/app/skin.yml",
      "inst/app/www/customer/favicon.png"
    )
  )
  run_test_git(repository, c("commit", "-m", shQuote("add downstream skin")))

  repository
}

test_that("downstream Git evidence is returned for an allowed divergence", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))
  metadata_file <- file.path(repository, ".fluvial-app.yml")

  evidence <- validate_downstream_repository(
    file = metadata_file,
    repository = repository
  )

  expect_equal(evidence$schema_version, 1L)
  expect_equal(evidence$metadata$application_id, "floodplain-connectivity")
  expect_equal(
    evidence$git$origin$repository,
    "https://github.com/FluvialGeomorph/floodplain-connectivity.git"
  )
  expect_equal(evidence$git$upstream$push_repository, "DISABLED")
  expect_equal(evidence$git$upstream$release, "2026.07.25")
  expect_match(evidence$git$head, "^[0-9a-f]{40,64}$")
  expect_equal(evidence$application$schema_version, 1L)
  expect_equal(
    evidence$application$referenced_customer_assets,
    "inst/app/www/customer/favicon.png"
  )
  expect_setequal(
    evidence$git$changed_paths,
    c(
      ".fluvial-app.yml",
      "inst/app/skin.yml",
      "inst/app/www/customer/favicon.png"
    )
  )
})

test_that("operator check prints a concise success summary", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))

  expect_output(
    evidence <- check_downstream_repository(
      file.path(repository, ".fluvial-app.yml"),
      repository
    ),
    paste(
      "PASS: downstream repository preflight",
      "Application: floodplain-connectivity",
      "Customer repository: https://github.com/FluvialGeomorph/",
      "Upstream release: 2026.07.25",
      "Referenced customer assets: 1",
      sep = ".*"
    )
  )
  expect_equal(evidence$metadata$application_id, "floodplain-connectivity")
})

test_that("downstream repository must have a clean working tree", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))
  writeLines("uncommitted", file.path(repository, "untracked.txt"))

  expect_error(
    validate_downstream_repository(
      file.path(repository, ".fluvial-app.yml"),
      repository
    ),
    "clean working tree"
  )
})

test_that("downstream metadata must be the tracked repository-root file", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))
  external_metadata <- tempfile(fileext = ".yml")
  yaml::write_yaml(git_test_downstream_metadata(), external_metadata)
  withr::defer(unlink(external_metadata))

  expect_error(
    validate_downstream_repository(external_metadata, repository),
    "repository-root"
  )
})

test_that("downstream repository requires the canonical upstream remote", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))
  run_test_git(
    repository,
    c("remote", "set-url", "upstream", "https://example.com/ohwm2.git")
  )

  expect_error(
    validate_downstream_repository(
      file.path(repository, ".fluvial-app.yml"),
      repository
    ),
    "must resolve to"
  )
})

test_that("downstream repository protects customer push direction", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))
  metadata_file <- file.path(repository, ".fluvial-app.yml")

  run_test_git(
    repository,
    c(
      "remote",
      "set-url",
      "--push",
      "upstream",
      "https://github.com/FluvialGeomorph/ohwm2.git"
    )
  )
  expect_error(
    validate_downstream_repository(metadata_file, repository),
    "protected push URL"
  )

  run_test_git(
    repository,
    c("remote", "set-url", "--push", "upstream", "DISABLED")
  )
  run_test_git(repository, c("config", "remote.pushDefault", "upstream"))
  expect_error(
    validate_downstream_repository(metadata_file, repository),
    "remote.pushDefault"
  )
})

test_that("downstream repository requires a distinct customer origin", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))
  metadata_file <- file.path(repository, ".fluvial-app.yml")

  run_test_git(
    repository,
    c(
      "remote",
      "set-url",
      "origin",
      "https://github.com/FluvialGeomorph/ohwm2.git"
    )
  )

  expect_error(
    validate_downstream_repository(metadata_file, repository),
    "customer repository"
  )
})

test_that("recorded upstream release must resolve locally", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))
  metadata_file <- file.path(repository, ".fluvial-app.yml")
  metadata <- yaml::read_yaml(metadata_file)
  metadata$upstream$release <- "2026.08.01"
  yaml::write_yaml(metadata, metadata_file)
  run_test_git(repository, c("add", ".fluvial-app.yml"))
  run_test_git(repository, c("commit", "-m", shQuote("record missing release")))

  expect_error(
    validate_downstream_repository(metadata_file, repository),
    "resolve upstream release"
  )
})

test_that("recorded upstream release must be an ancestor of HEAD", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))
  metadata_file <- file.path(repository, ".fluvial-app.yml")

  run_test_git(repository, c("checkout", "--orphan", "unrelated"))
  run_test_git(
    repository,
    c("commit", "-m", shQuote("unrelated release"))
  )
  run_test_git(repository, c("tag", "unrelated-release"))
  run_test_git(repository, c("checkout", "main"))
  metadata <- git_test_downstream_metadata()
  metadata$upstream$release <- "unrelated-release"
  yaml::write_yaml(metadata, metadata_file)
  run_test_git(repository, c("add", ".fluvial-app.yml"))
  run_test_git(repository, c("commit", "-m", shQuote("record unrelated release")))

  expect_error(
    validate_downstream_repository(metadata_file, repository),
    "not an ancestor"
  )
})

test_that("downstream shared-code divergence is rejected", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))
  dir.create(file.path(repository, "R"))
  writeLines("customer_patch <- TRUE", file.path(repository, "R", "patch.R"))
  run_test_git(repository, c("add", "R/patch.R"))
  run_test_git(repository, c("commit", "-m", shQuote("add customer patch")))

  expect_error(
    validate_downstream_repository(
      file.path(repository, ".fluvial-app.yml"),
      repository
    ),
    "outside owned paths.*R/patch.R"
  )
})

test_that("downstream skin rejects a missing customer asset", {
  repository <- create_downstream_repository()
  withr::defer(unlink(repository, recursive = TRUE))
  run_test_git(
    repository,
    c("rm", "inst/app/www/customer/favicon.png")
  )
  run_test_git(repository, c("commit", "-m", shQuote("remove customer asset")))

  expect_error(
    validate_downstream_repository(
      file.path(repository, ".fluvial-app.yml"),
      repository
    ),
    "missing packaged asset"
  )
})
