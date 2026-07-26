test_that("prepare_results_workflow_state returns slider state and readiness", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  res <- prepare_results_workflow_state(
    xs_pts = xs_pts,
    pick_xs = 1,
    channel_elevation = 103.5,
    floodplain_elevation = 112.25
  )

  expect_true(is.list(res))
  expect_true(is.list(res$slider_state))
  expect_true(res$results_loaded)
  expect_equal(res$slider_state$channel_elevation_value, 103.0)
  expect_equal(res$slider_state$floodplain_elevation_value, 103.0)
  expect_equal(res$slider_state$rem_min, 101.3)
  expect_equal(res$slider_state$rem_max, 103.0)
})

test_that("prepare_results_workflow_state preserves pick_xs selection", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  res <- prepare_results_workflow_state(
    xs_pts = xs_pts,
    pick_xs = 2,
    channel_elevation = 108.5,
    floodplain_elevation = 115.25
  )

  expect_equal(res$slider_state$rem_min, 110.2)
  expect_equal(res$slider_state$rem_max, 112.0)
  expect_equal(res$slider_state$channel_elevation_value, 110.2)
  expect_equal(res$slider_state$floodplain_elevation_value, 112.0)
  expect_true(res$results_loaded)
})

test_that("prepare_results_workflow_state errors when required columns are missing", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1)
  )

  expect_error(
    prepare_results_workflow_state(
      xs_pts = xs_pts,
      pick_xs = 1,
      channel_elevation = 103.5,
      floodplain_elevation = 112.25
    ),
    "Detrend_DEM_Z"
  )
})
