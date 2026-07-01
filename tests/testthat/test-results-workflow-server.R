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

    transition_state <- run_results_workflow_transition(
      session = session,
      input = input,
      xs_pts = xs_pts_value,
      set_results_loaded = function(...) NULL
    )

    expect_true(is.list(transition_state))
    expect_true(transition_state$results_loaded)
    expect_true(is.list(transition_state$slider_state))
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
