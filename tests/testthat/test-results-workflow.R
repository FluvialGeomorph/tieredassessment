test_that("app server exposes the expected server signature", {
  fmls <- formals(app_server)
  expect_true(all(c("input", "output", "session") %in% names(fmls)))
})

test_that("Results workflow gate starts FALSE", {
  skip_if_not_installed("shiny")

  shiny::testServer(app_server, {
    expect_false(results_loaded())
  })
})