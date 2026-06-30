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
      results_loaded = results_loaded
    )

    expect_true(results_loaded())
    expect_true(is.list(transition_state$workflow_state))
    expect_true(transition_state$workflow_state$results_loaded)
    expect_equal(
      transition_state$workflow_state$slider_state$channel_elevation_value,
      108.5
    )
    expect_equal(
      transition_state$workflow_state$slider_state$floodplain_elevation_value,
      115.25
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
    expect_false(results_loaded())

    session$setInputs(
      channel_elevation = 110.5,
      floodplain_elevation = 111.5,
      channel_mannings = 0.05,
      floodplain_mannings = 0.07,
      pick_xs = 2
    )

    xs_pts <<- xs_pts_value
    fl_editor_ui <<- function() list(finished = xs_pts_value)

    warn_msg <- NULL
    transition_state <- withCallingHandlers(
      run_results_workflow_transition(
        session = session,
        input = input,
        xs_pts = xs_pts_value,
        results_loaded = results_loaded
      ),
      warning = function(w) {
        warn_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )

    expect_true(results_loaded())
    expect_equal(input$pick_xs, 2)
    expect_true(is.list(transition_state$workflow_state))
    expect_true(transition_state$workflow_state$results_loaded)
    expect_null(warn_msg)

    slider_state <- prepare_results_slider_state(
      xs_pts = xs_pts_value,
      pick_xs = 2,
      channel_elevation = 110.5,
      floodplain_elevation = 111.5
    )

    expect_equal(slider_state$rem_min, 110.2)
    expect_equal(slider_state$rem_max, 112.0)
    expect_true(slider_state$channel_elevation_value <= slider_state$rem_max)
    expect_true(slider_state$floodplain_elevation_value <= slider_state$rem_max)
  })
})

test_that("repeat Results runs stay stable across fresh sessions", {
  skip_if_not_installed("shiny")

  xs_pts_value <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  run_once <- function() {
    shiny::testServer(app_server, {
      session$setInputs(
        channel_elevation = 110.5,
        floodplain_elevation = 111.5,
        channel_mannings = 0.05,
        floodplain_mannings = 0.07,
        pick_xs = 2
      )

      xs_pts <<- xs_pts_value
      fl_editor_ui <<- function() list(finished = xs_pts_value)

      transition_state <- run_results_workflow_transition(
        session = session,
        input = input,
        xs_pts = xs_pts_value,
        results_loaded = results_loaded
      )

      expect_true(results_loaded())
      expect_true(transition_state$workflow_state$results_loaded)
      expect_equal(transition_state$pick_xs, 2)
      expect_equal(
        transition_state$workflow_state$slider_state$channel_elevation_value,
        110.5
      )
      expect_equal(
        transition_state$workflow_state$slider_state$floodplain_elevation_value,
        111.5
      )
    })
  }

  run_once()
  run_once()
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
  expect_gte(workflow_state$slider_state$channel_elevation_value, workflow_state$slider_state$rem_min)
  expect_lte(workflow_state$slider_state$channel_elevation_value, workflow_state$slider_state$rem_max)
})
