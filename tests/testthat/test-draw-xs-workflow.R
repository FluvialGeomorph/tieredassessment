test_that("Draw XS workflow state helper returns ready contract", {
  # TODO: replace with realistic fixture used by Draw XS path
  xs_input <- data.frame(
    Seq = c(1, 1, 1, 2, 2, 2),
    Detrend_DEM_Z = c(101.2, 102.4, 103.8, 110.1, 111.3, 112.7)
  )

  # TODO: replace helper + args with your Draw XS helper signature
  workflow_state <- prepare_draw_xs_workflow_state(
    xs_pts = xs_input,
    pick_xs = 2
  )

  expect_true(is.list(workflow_state))
  expect_true("draw_xs_loaded" %in% names(workflow_state))
  expect_true(is.logical(workflow_state$draw_xs_loaded))
  expect_true(workflow_state$draw_xs_loaded)
})
