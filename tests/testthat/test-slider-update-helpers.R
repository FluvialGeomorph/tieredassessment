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

test_that("REM updates accept Manning inputs from Shiny select controls", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  channel_state <- prepare_channel_elevation_update(
    channel_elevation = 111.0,
    pick_xs = 2,
    xs_pts = xs_pts_value,
    mannings_n = "0.05"
  )
  floodplain_state <- prepare_floodplain_elevation_update(
    floodplain_elevation = 112.0,
    pick_xs = 2,
    xs_pts = xs_pts_value,
    mannings_n = "0.07"
  )

  expect_identical(channel_state$mannings_n, 0.05)
  expect_identical(floodplain_state$mannings_n, 0.07)
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

test_that("Channel Manning's n update captures slider values correctly", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  update_state <- prepare_channel_mannings_update(
    channel_elevation = 111.0,
    channel_mannings = 0.04,
    pick_xs = 2,
    xs_pts = xs_pts_value
  )

  expect_equal(update_state$channel_elevation_value, 111.0)
  expect_equal(update_state$channel_mannings_value, 0.04)
  expect_equal(update_state$pick_xs, 2)
})

test_that("Floodplain Manning's n update captures slider values correctly", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  update_state <- prepare_floodplain_mannings_update(
    floodplain_elevation = 112.5,
    floodplain_mannings = 0.07,
    pick_xs = 2,
    xs_pts = xs_pts_value
  )

  expect_equal(update_state$floodplain_elevation_value, 112.5)
  expect_equal(update_state$floodplain_mannings_value, 0.07)
  expect_equal(update_state$pick_xs, 2)
})

test_that("Manning updates accept values from Shiny select controls", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  channel_state <- prepare_channel_mannings_update(
    channel_elevation = 111.0,
    channel_mannings = "0.04",
    pick_xs = 2,
    xs_pts = xs_pts_value
  )
  floodplain_state <- prepare_floodplain_mannings_update(
    floodplain_elevation = 112.0,
    floodplain_mannings = "0.07",
    pick_xs = 2,
    xs_pts = xs_pts_value
  )

  expect_identical(channel_state$channel_mannings_value, 0.04)
  expect_identical(floodplain_state$floodplain_mannings_value, 0.07)
})

test_that("Manning's n value validation works for valid values", {
  # Typical values from app UI
  valid <- is_mannings_n_valid(0.03)
  expect_true(valid)

  valid <- is_mannings_n_valid(0.05)
  expect_true(valid)

  valid <- is_mannings_n_valid(0.07)
  expect_true(valid)

  valid <- is_mannings_n_valid(0.1)
  expect_true(valid)
})

test_that("Manning's n value validation rejects out-of-range values", {
  # Too low
  valid <- is_mannings_n_valid(0.005)
  expect_false(valid)

  # Too high
  valid <- is_mannings_n_valid(0.2)
  expect_false(valid)
})

test_that("Manning's n value validation rejects non-numeric values", {
  valid <- is_mannings_n_valid("0.05")
  expect_false(valid)

  valid <- is_mannings_n_valid(NULL)
  expect_false(valid)

  valid <- is_mannings_n_valid(c(0.05, 0.06))
  expect_false(valid)

  expect_false(is_mannings_n_valid(NA_real_))
  expect_false(is_mannings_n_valid(Inf))
  expect_error(
    normalize_mannings_n_input("not-a-number"),
    "must be one numeric value",
    fixed = TRUE
  )
})

test_that("Channel Manning's n update rejects missing required data", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  # Missing xs_pts should fail
  expect_error(
    prepare_channel_mannings_update(
      channel_elevation = 111.0,
      channel_mannings = 0.04,
      pick_xs = 2,
      xs_pts = NULL
    )
  )

  # Invalid cross-section should fail
  expect_error(
    prepare_channel_mannings_update(
      channel_elevation = 111.0,
      channel_mannings = 0.04,
      pick_xs = 99,
      xs_pts = xs_pts_value
    )
  )
})

test_that("Floodplain Manning's n update rejects missing required data", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  # Missing mannings value should fail
  expect_error(
    prepare_floodplain_mannings_update(
      floodplain_elevation = 112.5,
      floodplain_mannings = NULL,
      pick_xs = 2,
      xs_pts = xs_pts_value
    )
  )

  # Invalid cross-section should fail
  expect_error(
    prepare_floodplain_mannings_update(
      floodplain_elevation = 112.5,
      floodplain_mannings = 0.07,
      pick_xs = 99,
      xs_pts = xs_pts_value
    )
  )
})
