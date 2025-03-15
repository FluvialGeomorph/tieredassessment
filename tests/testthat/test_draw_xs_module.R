test_that("outpu instructionst", {  
  testServer(draw_xs_server, args = list(), {
    # test outputs
    #print(session$output$draw_xs_instructions$html)
    expect_true(is.character(session$output$draw_xs_instructions$html))
    
    # test variables inside server function
    #print(xs())
    expect_true("sf" %in% class(xs()))
    #print(class(draw_xs_map))
    expect_true("leaflet" %in% class(draw_xs_map))
    
    #print(draw_xs())
    
    # test return values
    print(session$getReturned())
    
    #new_xs <- draw_xs_server("1")
    #print(new_xs)
  })
})

test_that("", {
  xs <- callModule(draw_xs_server, "1")
})

test_that("", {
  # create reactives
  xs <- reactiveVal()
  
  testServer(draw_xs_server, args = list(), {
    # set reactives
    x <- ""
    session$flushReact()
    
    expect_true(is.character(session$output$draw_xs_instructions$html))
  })
})
