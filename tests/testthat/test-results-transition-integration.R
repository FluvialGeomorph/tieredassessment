# tests/testthat/test-results-transition-integration.R

test_that("Results transition completes without error", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  # Verify the workflow state preparation succeeds
  workflow_state <- prepare_results_workflow_state(
    xs_pts = xs_pts_value,
    pick_xs = 2,
    channel_elevation = 110.5,
    floodplain_elevation = 111.5
  )

  expect_true(workflow_state$results_loaded)
  expect_true(is.list(workflow_state$slider_state))
})

test_that("Slider bounds are computed correctly for selected cross-section", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  # Cross-section 2 has Detrend_DEM_Z = c(110.1, 111.3, 112.7)
  slider_state <- prepare_results_slider_state(
    xs_pts = xs_pts_value,
    pick_xs = 2,
    channel_elevation = 110.5,
    floodplain_elevation = 111.5
  )

  # Min: round(110.1, 1) + 0.1 = 110.2
  expect_equal(slider_state$rem_min, 110.2)
  # Max: round(112.7, 0) - 1 = 112.0
  expect_equal(slider_state$rem_max, 112.0)
})

test_that("Slider values are captured and preserved", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  slider_state <- prepare_results_slider_state(
    xs_pts = xs_pts_value,
    pick_xs = 2,
    channel_elevation = 110.5,
    floodplain_elevation = 111.5
  )

  expect_equal(slider_state$channel_elevation_value, 110.5)
  expect_equal(slider_state$floodplain_elevation_value, 111.5)
})

test_that("Slider values remain valid within computed bounds", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  slider_state <- prepare_results_slider_state(
    xs_pts = xs_pts_value,
    pick_xs = 2,
    channel_elevation = 110.5,
    floodplain_elevation = 111.5
  )

  # Verify values are within their bounds (safety check for the workflow)
  expect_gte(slider_state$channel_elevation_value, slider_state$rem_min)
  expect_lte(slider_state$channel_elevation_value, slider_state$rem_max)
  expect_gte(slider_state$floodplain_elevation_value, slider_state$rem_min)
  expect_lte(slider_state$floodplain_elevation_value, slider_state$rem_max)
})

test_that("Cross-section selection works across different cross-sections", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  # Test cross-section 1
  slider_state_xs1 <- prepare_results_slider_state(
    xs_pts = xs_pts_value,
    pick_xs = 1,
    channel_elevation = 102.0,
    floodplain_elevation = 103.5
  )

  # XS 1: min = round(101.2, 1) + 0.1 = 101.3; max = round(103.8, 0) - 1 = 103.0
  expect_equal(slider_state_xs1$rem_min, 101.3)
  expect_equal(slider_state_xs1$rem_max, 103.0)

  # Test cross-section 2
  slider_state_xs2 <- prepare_results_slider_state(
    xs_pts = xs_pts_value,
    pick_xs = 2,
    channel_elevation = 110.5,
    floodplain_elevation = 111.5
  )

  # XS 2: min = round(110.1, 1) + 0.1 = 110.2; max = round(112.7, 0) - 1 = 112.0
  expect_equal(slider_state_xs2$rem_min, 110.2)
  expect_equal(slider_state_xs2$rem_max, 112.0)

  # Verify they differ appropriately
  expect_lt(slider_state_xs1$rem_max, slider_state_xs2$rem_min)
})

test_that("Results loaded flag is set correctly after workflow state preparation", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  workflow_state <- prepare_results_workflow_state(
    xs_pts = xs_pts_value,
    pick_xs = 2,
    channel_elevation = 110.5,
    floodplain_elevation = 111.5
  )

  expect_true(workflow_state$results_loaded)
})

test_that("Results workflow state preparation remains stable across repeated runs", {
  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  # First run
  first <- prepare_results_workflow_state(
    xs_pts = xs_pts_value,
    pick_xs = 2,
    channel_elevation = 110.5,
    floodplain_elevation = 111.5
  )

  # Second run with the same inputs
  second <- prepare_results_workflow_state(
    xs_pts = xs_pts_value,
    pick_xs = 2,
    channel_elevation = 110.5,
    floodplain_elevation = 111.5
  )

  # Workflow readiness remains true
  expect_true(first$results_loaded)
  expect_true(second$results_loaded)

  # Core slider-state invariants remain identical and valid
  expect_equal(second$slider_state$rem_min, first$slider_state$rem_min)
  expect_equal(second$slider_state$rem_max, first$slider_state$rem_max)
  expect_equal(second$slider_state$channel_elevation_value, first$slider_state$channel_elevation_value)
  expect_equal(second$slider_state$floodplain_elevation_value, first$slider_state$floodplain_elevation_value)

  # Safety bounds still hold after repeat execution
  expect_gte(second$slider_state$channel_elevation_value, second$slider_state$rem_min)
  expect_lte(second$slider_state$channel_elevation_value, second$slider_state$rem_max)
})
