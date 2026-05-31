test_that("prepare_results_slider_state captures values and computes range", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 104.1, 105.3, 106.7)
  )

  res <- prepare_results_slider_state(
    xs_pts = xs_pts,
    pick_xs = 1,
    channel_elevation = 103.5,
    floodplain_elevation = 112.25
  )

  expect_equal(res$channel_elevation_value, 103.5)
  expect_equal(res$floodplain_elevation_value, 112.25)
  expect_equal(res$rem_min, 101.3)
  expect_equal(res$rem_max, 103.0)
})

test_that("prepare_results_slider_state enforces required columns", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1),
    SomeOtherColumn = c(1, 2, 3)
  )

  expect_error(
    prepare_results_slider_state(
      xs_pts = xs_pts,
      pick_xs = 1,
      channel_elevation = 103.5,
      floodplain_elevation = 112.25
    ),
    "Detrend_DEM_Z"
  )
})

test_that("prepare_results_slider_state respects pick_xs selection", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  res <- prepare_results_slider_state(
    xs_pts = xs_pts,
    pick_xs = 2,
    channel_elevation = 108,
    floodplain_elevation = 115
  )

  expect_equal(res$rem_min, 110.2)
  expect_equal(res$rem_max, 112.0)
})

test_that("prepare_results_slider_state clamps rem_min at 100", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1),
    Detrend_DEM_Z = c(90.1, 91.4, 92.6)
  )

  res <- prepare_results_slider_state(
    xs_pts = xs_pts,
    pick_xs = 1,
    channel_elevation = 103.5,
    floodplain_elevation = 112.25
  )

  expect_equal(res$rem_min, 100)
})
