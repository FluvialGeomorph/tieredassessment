test_that("Draw XS transition calls injected gate setter with readiness", {
  skip_if_not_installed("shiny")

  # TODO: replace with real fixture required by Draw XS transition
  xs_input <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  shiny::testServer(app_server, {
    # TODO: set all required Draw XS inputs here
    session$setInputs(
      pick_xs = 2
    )

    gate_calls <- list()
    capture_gate <- function(value) {
      gate_calls[[length(gate_calls) + 1]] <<- value
    }

    # TODO: replace function name/args to match Draw XS transition seam
    transition_state <- run_draw_xs_workflow_transition(
      session = session,
      input = input,
      xs_pts = xs_input,
      set_draw_xs_loaded = capture_gate
    )

    expect_true(is.list(transition_state))
    expect_true(isTRUE(transition_state$draw_xs_loaded))
    expect_length(gate_calls, 1)
    expect_identical(gate_calls[[1]], TRUE)
  })
})
