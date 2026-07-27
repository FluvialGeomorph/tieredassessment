test_that("template default skin preserves the OHWM presentation", {
  skin <- read_app_skin_file(app_sys("app/skin-default.yml"))
  skin <- normalize_app_skin(skin)
  validate_app_skin(skin)

  expect_equal(skin$schema_version, 1)
  expect_equal(skin$identity$app_title, "Ordinary High Water Marks")
  expect_equal(skin$theme$bootswatch, "sandstone")
  expect_equal(skin$workflow$draw_xs$nav_label, "Draw XS")
  expect_equal(skin$workflow$draw_flowline$nav_label, "Draw Flowline")
  expect_equal(skin$workflow$results$nav_label, "Results")
  expect_equal(
    skin$workflow$results$progress_message,
    "Preparing Slope and Discharge Data"
  )
})

test_that("partial downstream skin overrides inherit template defaults", {
  override_file <- tempfile(fileext = ".yml")
  withr::defer(unlink(override_file))
  writeLines(
    c(
      "default:",
      "  identity:",
      "    app_title: Floodplain Connectivity",
      "    browser_title: Floodplain Connectivity",
      "  workflow:",
      "    draw_xs:",
      "      nav_label: Locate Cross Sections"
    ),
    override_file
  )

  skin <- load_app_skin(override_file = override_file)

  expect_equal(skin$identity$app_title, "Floodplain Connectivity")
  expect_equal(skin$identity$favicon, "www/favicon.png")
  expect_equal(skin$workflow$draw_xs$nav_label, "Locate Cross Sections")
  expect_equal(
    skin$workflow$draw_xs$next_button,
    "Draw Flowline"
  )
  expect_equal(skin$workflow$results$nav_label, "Results")
  expect_equal(
    skin$workflow$results$progress_message,
    "Preparing Slope and Discharge Data"
  )
})

test_that("environment variable can select a downstream skin override", {
  override_file <- tempfile(fileext = ".yml")
  withr::defer(unlink(override_file))
  writeLines(
    c(
      "default:",
      "  identity:",
      "    app_title: Environment Selected Skin",
      "    browser_title: Environment Selected Skin"
    ),
    override_file
  )
  withr::local_envvar(
    c(FLUVIAL_APP_SKIN_FILE = override_file)
  )

  skin <- load_app_skin()

  expect_equal(skin$identity$app_title, "Environment Selected Skin")
})

test_that("exported skin preflight uses the application startup path", {
  override_file <- tempfile(fileext = ".yml")
  withr::defer(unlink(override_file))
  writeLines(
    c(
      "default:",
      "  identity:",
      "    app_title: Preflight Skin",
      "    browser_title: Preflight Skin"
    ),
    override_file
  )

  skin <- validate_app_skin_file(override_file)

  expect_equal(skin$identity$app_title, "Preflight Skin")
  expect_equal(skin$workflow$results$nav_label, "Results")
})

test_that("skin validation rejects unknown fields", {
  override_file <- tempfile(fileext = ".yml")
  withr::defer(unlink(override_file))
  writeLines(
    c(
      "default:",
      "  workflow:",
      "    draw_xs:",
      "      hidden_behavior_switch: true"
    ),
    override_file
  )

  expect_error(
    load_app_skin(override_file = override_file),
    "unknown field"
  )
})

test_that("skin validation rejects unsupported schema versions", {
  override_file <- tempfile(fileext = ".yml")
  withr::defer(unlink(override_file))
  writeLines(
    c(
      "default:",
      "  schema_version: 2"
    ),
    override_file
  )

  expect_error(
    load_app_skin(override_file = override_file),
    "supported value 1"
  )
})

test_that("skin validation rejects missing packaged assets", {
  skin <- read_app_skin_file(app_sys("app/skin-default.yml"))
  skin <- normalize_app_skin(skin)
  skin$identity$favicon <- "www/missing-favicon.png"

  expect_error(
    validate_app_skin(skin),
    "missing packaged asset"
  )
})

test_that("app UI uses visible skin labels and stable navigation values", {
  withr::local_options(list(sass.cache = FALSE))
  skin <- read_app_skin_file(app_sys("app/skin-default.yml"))
  skin <- normalize_app_skin(skin)
  skin$identity$app_title <- "Tiered Assessment"
  skin$workflow$draw_xs$nav_label <- "Define Assessment Sections"
  skin$workflow$draw_flowline$nav_label <- "Define Reach"
  skin$workflow$results$nav_label <- "Assessment Results"

  html <- htmltools::renderTags(app_ui(skin = skin))$html

  expect_match(html, "Tiered Assessment", fixed = TRUE)
  expect_match(html, "Define Assessment Sections", fixed = TRUE)
  expect_match(html, "Define Reach", fixed = TRUE)
  expect_match(html, "Assessment Results", fixed = TRUE)
  expect_match(html, 'data-value="draw_xs"', fixed = TRUE)
  expect_match(html, 'data-value="draw_flowline"', fixed = TRUE)
  expect_match(html, 'data-value="results"', fixed = TRUE)
  expect_match(html, 'id="flowline_editor_ui"', fixed = TRUE)
  expect_match(
    html,
    'class="shiny-html-output html-fill-item html-fill-container" id="flowline_editor_ui"',
    fixed = TRUE
  )
  expect_match(html, 'id="slope_scale"', fixed = TRUE)
  expect_match(html, "USGS Reach (recommended)", fixed = TRUE)
  expect_match(html, 'value="usgs_reach" selected', fixed = TRUE)
  expect_match(html, "Sampled DEM Reach", fixed = TRUE)
  expect_match(html, 'value="dem_reach"', fixed = TRUE)
  expect_match(html, "Local XS Neighborhood", fixed = TRUE)
  expect_match(html, 'value="dem_xs_local"', fixed = TRUE)
  expect_match(html, 'id="interactive_flooding_status"', fixed = TRUE)
})

test_that("run_app does not accept both a skin and a skin file", {
  skin <- read_app_skin_file(app_sys("app/skin-default.yml"))
  skin <- normalize_app_skin(skin)

  expect_error(
    run_app(skin = skin, skin_file = "skin.yml"),
    "Supply only one"
  )
})
