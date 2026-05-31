test_that("app server exposes the Results workflow inputs and outputs", {
  fmls <- formals(app_server)
  expect_true(all(c("input", "output", "session") %in% names(fmls)))

  # These are UI-level contract checks derived from app_ui()
  ui <- app_ui()

  # We don't try to fully render the UI here; this is a light contract check
  # that the top-level app object is still constructible.
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("Results workflow state gate is initialized to FALSE", {
  skip_if_not_installed("shiny")

  shiny::testServer(app_server, {
    # The app should start in an uninitialized state.
    expect_false(results_loaded())
  })
})

test_that("Results UI controls exist in the app UI contract", {
  ui <- app_ui()

  # This is intentionally lightweight: we verify the UI declares the core
  # Results controls needed by the workflow.
  ui_html <- paste(capture.output(print(ui)), collapse = "\n")

  expect_match(ui_html, 'inputId = "pick_xs"', fixed = TRUE)
  expect_match(ui_html, 'inputId = "channel_elevation"', fixed = TRUE)
  expect_match(ui_html, 'inputId = "floodplain_elevation"', fixed = TRUE)
  expect_match(ui_html, 'inputId = "channel_mannings"', fixed = TRUE)
  expect_match(ui_html, 'inputId = "floodplain_mannings"', fixed = TRUE)
  expect_match(ui_html, 'leafletOutput("results_map")', fixed = TRUE)
})

test_that("Results renderers are guarded by results_loaded()", {
  server_src <- paste(readLines("R/app_server.R", warn = FALSE), collapse = "\n")

  expect_match(server_src, "req\\(results_loaded\\(\\)\\)", perl = TRUE)
  expect_match(server_src, "results_loaded\\(TRUE\\)", perl = TRUE)
})

test_that("Results workflow uses captured local slider values before programmatic updates", {
  server_src <- paste(readLines("R/app_server.R", warn = FALSE), collapse = "\n")

  # The bug we fixed should remain impossible by convention:
  # capture inputs first, then update sliders.
  expect_match(server_src, "channel_elevation_value <- input\\$channel_elevation", perl = TRUE)
  expect_match(server_src, "floodplain_elevation_value <- input\\$floodplain_elevation", perl = TRUE)
  expect_match(server_src, "freezeReactiveValue\\(input, \"channel_elevation\"\\)", perl = TRUE)
  expect_match(server_src, "freezeReactiveValue\\(input, \"floodplain_elevation\"\\)", perl = TRUE)
})

test_that("Results workflow preserves the explicit readiness gate pattern", {
  server_src <- paste(readLines("R/app_server.R", warn = FALSE), collapse = "\n")

  # Confirm that the gating pattern is still present for outputs that should not
  # render until Results is ready.
  expect_match(server_src, "req\\(results_loaded\\(\\)\\)", perl = TRUE)
  expect_match(server_src, "renderPlot\\(\\{\\s*req\\(results_loaded\\(\\)\\)", perl = TRUE)
  expect_match(server_src, "render_gt\\(\\{\\s*req\\(results_loaded\\(\\)\\)", perl = TRUE)
})

test_that("The app still defines the Results workflow output bindings", {
  server_src <- paste(readLines("R/app_server.R", warn = FALSE), collapse = "\n")

  expect_match(server_src, "output\\$results_map <- renderLeaflet", perl = TRUE)
  expect_match(server_src, "output\\$long_profile <- renderPlot", perl = TRUE)
  expect_match(server_src, "output\\$xs_plot_floodplain <- renderPlot", perl = TRUE)
  expect_match(server_src, "output\\$xs_plot_channel <- renderPlot", perl = TRUE)
  expect_match(server_src, "output\\$floodplain_volumes <- render_gt", perl = TRUE)
  expect_match(server_src, "output\\$channel_discharge <- render_gt", perl = TRUE)
  expect_match(server_src, "output\\$floodplain_discharge <- render_gt", perl = TRUE)
})
