draw_xs_ui <- function(id) {
  tagList(
    layout_sidebar(
      editModUI(NS(id, "xs_editor")),
      sidebar = sidebar(
        title = "Draw XS Instructions",
        position = "right", width = "25%",
        uiOutput(NS(id, "draw_xs_instructions")),
        uiOutput(NS(id, "get_terrain_button"))
      )
    )
  )
}

draw_xs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$draw_xs_instructions <- renderUI({
      steps <- c('Zoom to the desired AOI.', 
                 'Draw cross sections beginning with the most downstream cross section first.', 
                 'Click the "Get Terrain" button below to retrieve the digital elevation model (DEM).',
                 'View the terrain using the "View Terrain" button or top menu.')
      ul <- htmltools::tags$ul(
        purrr::map(steps, function(.x) tags$li(.x)))
    })
    
    xs <- reactive({
      sf <- data.frame(Seq = integer()) %>%
        st_as_sf(geometry = st_sfc(), 
                 crs = 3857)  # ensure Web Mercator
      print(xs)
    })
    
    draw_xs_map <- leaflet() %>%  # defaults to Web Mercator
      setView(lng = -93.85, lat = 37.45, zoom = 13) %>%
      addProviderTiles("USGS.USTopo")
    
    draw_xs <- callModule(
      editMod,
      id = "xs_editor",
      leafmap = draw_xs_map,
      targetLayerId = xs,
      crs = 3857,
      editor = "leafpm",
      editorOptions = list(
        toolbarOptions = pmToolbarOptions(
          drawMarker = FALSE,
          drawPolygon = FALSE,
          drawCircle = FALSE,
          drawRectangle = FALSE,
          cutPolygon = FALSE,
          position = "topright")
      ))
    
    observeEvent(draw_xs()$finished, {
      new_xs <- sf::st_transform(draw_xs()$finished, crs = 3857) # Web Mercator
      print(new_xs)
      output$get_terrain_button <- renderUI({
        actionButton("get_terrain", "Get Terrain")
      })
      list(xs = reactive(new_xs))
    })
  })
}

draw_xs_app <- function() {
  ui <- page_navbar(title = "Main App", id = "main",
    nav_panel( title = "Draw XS",
      draw_xs_ui("xs_editor"),
    )
  )
  server <- function(input, output, session) {
    xs <- draw_xs_server("xs_editor")
  }
  shinyApp(ui, server)
}