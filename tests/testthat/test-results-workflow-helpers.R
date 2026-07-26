test_that("prepare_results_slider_state computes range and clamps values", {
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

  expect_equal(res$channel_elevation_value, 103.0)
  expect_equal(res$floodplain_elevation_value, 103.0)
  expect_equal(res$rem_min, 101.3)
  expect_equal(res$rem_max, 103.0)
})

test_that("prepare_results_slider_state accepts production sf points", {
  xs_pts <- sf::st_as_sf(
    data.frame(
      Seq = c(1, 1, 1, 2, 2, 2),
      Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7),
      x = seq_len(6),
      y = seq_len(6)
    ),
    coords = c("x", "y"),
    crs = 3857
  )

  res <- prepare_results_slider_state(
    xs_pts = xs_pts,
    pick_xs = 2,
    channel_elevation = 110.5,
    floodplain_elevation = 111.5
  )

  expect_equal(res$rem_min, 110.2)
  expect_equal(res$rem_max, 112.0)
  expect_equal(res$channel_elevation_value, 110.5)
  expect_equal(res$floodplain_elevation_value, 111.5)
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

test_that("prepare_results_slider_state rejects nonnumeric elevations", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1),
    Detrend_DEM_Z = c("101.2", "102.4", "103.8")
  )

  expect_error(
    prepare_results_slider_state(
      xs_pts = xs_pts,
      pick_xs = 1,
      channel_elevation = 102,
      floodplain_elevation = 103
    ),
    "must be a numeric vector",
    fixed = TRUE
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
    Detrend_DEM_Z = c(99.1, 100.4, 102.6)
  )

  res <- prepare_results_slider_state(
    xs_pts = xs_pts,
    pick_xs = 1,
    channel_elevation = 103.5,
    floodplain_elevation = 112.25
  )

  expect_equal(res$rem_min, 100)
  expect_equal(res$rem_max, 102)
})

test_that("prepare_results_slider_state clamps values to computed bounds", {
  xs_pts <- data.frame(
    Seq = c(1, 1, 1),
    Detrend_DEM_Z = c(110.1, 111.3, 112.7)
  )

  res <- prepare_results_slider_state(
    xs_pts = xs_pts,
    pick_xs = 1,
    channel_elevation = 108.5,
    floodplain_elevation = 115.25
  )

  expect_equal(res$rem_min, 110.2)
  expect_equal(res$rem_max, 112.0)
  expect_equal(res$channel_elevation_value, res$rem_min)
  expect_equal(res$floodplain_elevation_value, res$rem_max)
})

test_that("prepare_results_slider_state rejects unusable slider state", {
  no_finite_values <- data.frame(
    Seq = c(1, 1),
    Detrend_DEM_Z = c(NA_real_, Inf)
  )
  expect_error(
    prepare_results_slider_state(
      xs_pts = no_finite_values,
      pick_xs = 1,
      channel_elevation = 103,
      floodplain_elevation = 112
    ),
    "no finite"
  )

  no_valid_range <- data.frame(
    Seq = c(1, 1),
    Detrend_DEM_Z = c(100.1, 100.5)
  )
  expect_error(
    prepare_results_slider_state(
      xs_pts = no_valid_range,
      pick_xs = 1,
      channel_elevation = 103,
      floodplain_elevation = 112
    ),
    "valid Results slider range"
  )

  valid_xs <- data.frame(
    Seq = c(1, 1),
    Detrend_DEM_Z = c(101.1, 103.5)
  )
  expect_error(
    prepare_results_slider_state(
      xs_pts = valid_xs,
      pick_xs = 1,
      channel_elevation = NA_real_,
      floodplain_elevation = 102
    ),
    "channel_elevation"
  )
})
