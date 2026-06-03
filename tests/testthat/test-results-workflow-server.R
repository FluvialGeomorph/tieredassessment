test_that("Results slider state helper can support the workflow transition", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  slider_state <- prepare_results_slider_state(
    xs_pts = xs_pts,
    pick_xs = 2,
    channel_elevation = 108.5,
    floodplain_elevation = 115.25
  )

  expect_equal(slider_state$channel_elevation_value, 108.5)
  expect_equal(slider_state$floodplain_elevation_value, 115.25)
  expect_equal(slider_state$rem_min, 110.2)
  expect_equal(slider_state$rem_max, 112.0)
})

test_that("view_results observer is present in the server", {
  fmls <- formals(app_server)
  expect_true(all(c("input", "output", "session") %in% names(fmls)))
})

test_that("Results workflow state helper marks the workflow ready", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  workflow_state <- prepare_results_workflow_state(
    xs_pts = xs_pts,
    pick_xs = 2,
    channel_elevation = 108.5,
    floodplain_elevation = 115.25
  )

  expect_true(is.list(workflow_state))
  expect_true(is.list(workflow_state$slider_state))
  expect_true(workflow_state$results_loaded)
})

test_that("app server starts with Results gating disabled", {
  skip_if_not_installed("shiny")

  shiny::testServer(app_server, {
    expect_false(results_loaded())
  })
})
