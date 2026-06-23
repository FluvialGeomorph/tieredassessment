test_that("Channel elevation update captures slider values correctly", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7),
    channel = c(1, 1, 1, 1, 1, 1),
    ReachName = rep("Test Stream", 6)
  )

  update_state <- prepare_channel_elevation_update(
    channel_elevation = 111.0,
    pick_xs = 2,
    xs_pts = xs_pts_value,
    mannings_n = 0.05
  )

  expect_equal(update_state$channel_elevation_value, 111.0)
  expect_equal(update_state$pick_xs, 2)
  expect_equal(update_state$mannings_n, 0.05)
})

test_that("Floodplain elevation update captures slider values correctly", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7),
    channel = c(1, 1, 1, 1, 1, 1),
    ReachName = rep("Test Stream", 6)
  )

  update_state <- prepare_floodplain_elevation_update(
    floodplain_elevation = 112.5,
    pick_xs = 2,
    xs_pts = xs_pts_value,
    mannings_n = 0.07
  )

  expect_equal(update_state$floodplain_elevation_value, 112.5)
  expect_equal(update_state$pick_xs, 2)
  expect_equal(update_state$mannings_n, 0.07)
})

test_that("Elevation value validation works for valid values", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  # XS 2: min = 110.2, max = 112.0
  valid <- is_elevation_value_valid(111.0, xs_pts_value, 2)
  expect_true(valid)

  # XS 1: min = 101.3, max = 103.0
  valid <- is_elevation_value_valid(102.0, xs_pts_value, 1)
  expect_true(valid)
})

test_that("Elevation value validation rejects out-of-bounds values", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  # XS 2: min = 110.2, max = 112.0; 115 is too high
  valid <- is_elevation_value_valid(115.0, xs_pts_value, 2)
  expect_false(valid)

  # XS 1: min = 101.3, max = 103.0; 100 is too low
  valid <- is_elevation_value_valid(100.0, xs_pts_value, 1)
  expect_false(valid)
})

test_that("Elevation validation rejects invalid cross-section", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  # Cross-section 99 doesn't exist
  valid <- is_elevation_value_valid(111.0, xs_pts_value, 99)
  expect_false(valid)
})

test_that("Channel elevation update rejects missing required data", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7),
    channel = c(1, 1, 1, 1, 1, 1),
    ReachName = rep("Test Stream", 6)
  )

  # Missing xs_pts should fail
  expect_error(
    prepare_channel_elevation_update(
      channel_elevation = 111.0,
      pick_xs = 2,
      xs_pts = NULL,
      mannings_n = 0.05
    )
  )
})

test_that("Floodplain elevation update rejects missing required data", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7),
    channel = c(1, 1, 1, 1, 1, 1),
    ReachName = rep("Test Stream", 6)
  )

  # Missing mannings_n should fail
  expect_error(
    prepare_floodplain_elevation_update(
      floodplain_elevation = 112.5,
      pick_xs = 2,
      xs_pts = xs_pts_value,
      mannings_n = NULL
    )
  )
})
