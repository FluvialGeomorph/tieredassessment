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

  expect_equal(slider_state$channel_elevation_value, 110.2)
  expect_equal(slider_state$floodplain_elevation_value, 112.0)
  expect_equal(slider_state$rem_min, 110.2)
  expect_equal(slider_state$rem_max, 112.0)
})

test_that("view_results observer is present in the server", {
  fmls <- formals(app_server)
  expect_true(all(c("input", "output", "session") %in% names(fmls)))
  expect_true("reach_slope_resolver" %in% names(fmls))
  expect_true("dem_slope_resolver" %in% names(fmls))
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

test_that("post-flush Results gate is safe without an active reactive consumer", {
  skip_if_not_installed("shiny")

  results_gate <- shiny::reactiveVal(TRUE)

  expect_no_error(
    ready <- read_deferred_results_gate(results_gate)
  )
  expect_true(ready)

  results_gate(FALSE)
  expect_false(read_deferred_results_gate(results_gate))
})

test_that("discharge recalculation reuses the cached reach slope", {
  skip_if_not_installed("shiny")

  resolver_calls <- 0L
  resolver <- function(xs_pts, xs_number) {
    resolver_calls <<- resolver_calls + 1L
    new_reach_slope_result(
      value = 0.002,
      source = "usgs_nhdplus",
      status = "available",
      reason = NULL,
      attempts = 1L,
      message = "USGS NHDPlus reach slope is available."
    )
  }
  testthat::local_mocked_bindings(
    resolve_reach_slope = resolver,
    .package = "ohwm2"
  )

  shiny::testServer(app_server, {
      xs_pts_value <- fluvgeo::sin_riffle_channel_points_sf
      xs_pts_value$channel <- 1
      xs_pts <<- xs_pts_value
      session$setInputs(pick_xs = 4, slope_scale = "usgs_reach")

      refresh_dem_slope()
      refresh_reach_slope(notify_user = FALSE)
      expect_equal(resolver_calls, 1L)

      channel_table <- render_cached_discharge(
        xs_pts = xs_pts_value,
        xs_number = 4,
        bf_estimate = 103.5,
        mannings_n = 0.035
      )
      floodplain_table <- render_cached_discharge(
        xs_pts = xs_pts_value,
        xs_number = 4,
        bf_estimate = 104,
        mannings_n = 0.05
      )

      expect_s3_class(channel_table, "gt_tbl")
      expect_s3_class(floodplain_table, "gt_tbl")

      session$setInputs(slope_scale = "dem_local")
      local_table <- render_cached_discharge(
        xs_pts = xs_pts_value,
        xs_number = 4,
        bf_estimate = 103.5,
        mannings_n = 0.035
      )
      expect_s3_class(local_table, "gt_tbl")

      session$setInputs(slope_scale = "usgs_reach")
      expect_s3_class(
        render_cached_discharge(
          xs_pts = xs_pts_value,
          xs_number = 4,
          bf_estimate = 103.5,
          mannings_n = 0.035
        ),
        "gt_tbl"
      )
      expect_equal(resolver_calls, 1L)
  })
})

test_that("run_results_workflow_transition marks Results ready", {
  skip_if_not_installed("shiny")

  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  shiny::testServer(app_server, {
    session$setInputs(
      channel_elevation = 108.5,
      floodplain_elevation = 115.25,
      pick_xs = 2
    )

    expect_no_warning(
      transition_state <- run_results_workflow_transition(
        session = session,
        input = input,
        xs_pts = xs_pts_value,
        set_results_loaded = function(...) NULL
      )
    )

    expect_true(is.list(transition_state))
    expect_true(transition_state$results_loaded)
    expect_true(is.list(transition_state$slider_state))
    expect_equal(
      transition_state$slider_state$channel_elevation_value,
      110.2
    )
    expect_equal(
      transition_state$slider_state$floodplain_elevation_value,
      112.0
    )
  })
})

test_that("first Results run reaches the ready state", {
  skip_if_not_installed("shiny")

  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  shiny::testServer(app_server, {
    session$setInputs(
      channel_elevation = 110.5,
      floodplain_elevation = 111.5,
      pick_xs = 2
    )

    gate_calls <- list()
    capture_gate <- function(value) {
      gate_calls[[length(gate_calls) + 1]] <<- value
    }

    state1 <- run_results_workflow_transition(
      session = session,
      input = input,
      xs_pts = xs_pts_value,
      set_results_loaded = capture_gate
    )

    expect_true(state1$results_loaded)
    expect_true(is.list(state1$slider_state))
    expect_length(gate_calls, 1)
    expect_identical(gate_calls[[1]], TRUE)

    expect_equal(state1$slider_state$rem_min, 110.2)
    expect_equal(state1$slider_state$rem_max, 112.0)
    expect_gte(
      state1$slider_state$channel_elevation_value,
      state1$slider_state$rem_min
    )
    expect_lte(
      state1$slider_state$channel_elevation_value,
      state1$slider_state$rem_max
    )
  })
})

test_that("repeat Results runs stay stable across fresh sessions", {
  skip_if_not_installed("shiny")

  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  run_once <- function(xs_pts_value) {
    out <- NULL

    shiny::testServer(app_server, {
      session$setInputs(
        channel_elevation = 110.5,
        floodplain_elevation = 111.5,
        pick_xs = 2
      )

      gate_calls <- list()
      capture_gate <- function(value) {
        gate_calls[[length(gate_calls) + 1]] <<- value
      }

      state <- run_results_workflow_transition(
        session = session,
        input = input,
        xs_pts = xs_pts_value,
        set_results_loaded = capture_gate
      )

      out <<- list(
        ready = isTRUE(state$results_loaded),
        slider = state$slider_state,
        gate_calls = gate_calls
      )
    })

    out
  }

  first <- run_once(xs_pts_value)
  second <- run_once(xs_pts_value)

  expect_true(first$ready)
  expect_true(second$ready)

  expect_length(first$gate_calls, 1)
  expect_length(second$gate_calls, 1)
  expect_identical(first$gate_calls[[1]], TRUE)
  expect_identical(second$gate_calls[[1]], TRUE)

  expect_equal(second$slider$rem_min, first$slider$rem_min)
  expect_equal(second$slider$rem_max, first$slider$rem_max)
  expect_equal(
    second$slider$channel_elevation_value,
    first$slider$channel_elevation_value
  )
  expect_equal(
    second$slider$floodplain_elevation_value,
    first$slider$floodplain_elevation_value
  )
})

test_that("Workflow transition contract returns ready state and valid slider bounds", {
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
  expect_true(is.list(workflow_state$slider_state))
  expect_equal(workflow_state$slider_state$rem_min, 110.2)
  expect_equal(workflow_state$slider_state$rem_max, 112.0)
  expect_gte(
    workflow_state$slider_state$channel_elevation_value,
    workflow_state$slider_state$rem_min
  )
  expect_lte(
    workflow_state$slider_state$channel_elevation_value,
    workflow_state$slider_state$rem_max
  )
})

test_that("run_results_workflow_transition calls injected gate setter with readiness", {
  skip_if_not_installed("shiny")

  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  shiny::testServer(app_server, {
    session$setInputs(
      channel_elevation = 110.5,
      floodplain_elevation = 111.5,
      pick_xs = 2
    )

    gate_calls <- list()
    capture_gate <- function(value) {
      gate_calls[[length(gate_calls) + 1]] <<- value
    }

    transition_state <- run_results_workflow_transition(
      session = session,
      input = input,
      xs_pts = xs_pts_value,
      set_results_loaded = capture_gate
    )

    expect_true(transition_state$results_loaded)
    expect_length(gate_calls, 1)
    expect_identical(gate_calls[[1]], TRUE)
  })
})
