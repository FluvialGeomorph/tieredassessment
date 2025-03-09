test_that("", {
  # create reactives
  xs <- reactiveVal()
  
  testServer(draw_xs_server, args = list(), {
    # set reactives
    x <- ""
    session$flushReact()
    
    expect_equal()
  })
})