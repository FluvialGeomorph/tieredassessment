test_that("Draw XS repeat runs stay stable across fresh sessions", {
  skip_if_not_installed("shiny")

  xs_input <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  run_once <- function(xs_pts_value) {
    out <- NULL

    shiny::testServer(app_server, {
      # TODO: set required Draw XS inputs
      session$setInputs(pick_xs = 2)

      gate_calls <- list()
      capture_gate <- function(value) {
        gate_calls[[length(gate_calls) + 1]] <<- value
      }

      # TODO: replace function name/args to match Draw XS transition seam
      state <- run_draw_xs_workflow_transition(
        session = session,
        input = input,
        xs_pts = xs_pts_value,
        set_draw_xs_loaded = capture_gate
      )

      out <<- list(
        ready = isTRUE(state$draw_xs_loaded),
        gate_calls = gate_calls
        # TODO: add additional invariant fields to compare across runs
      )
    })

    out
  }

  first <- run_once(xs_input)
  second <- run_once(xs_input)

  expect_true(first$ready)
  expect_true(second$ready)

  expect_length(first$gate_calls, 1)
  expect_length(second$gate_calls, 1)
  expect_identical(first$gate_calls[[1]], TRUE)
  expect_identical(second$gate_calls[[1]], TRUE)
})
