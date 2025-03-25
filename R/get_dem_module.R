get_dem_ui <- function(id) {
  # ??
}

get_dem_server <- function(id, xs, get_terrain_button) {
  moduleServer(id, function(input, output, session) {
    dem <- reactive({
      raster <- matrix(1:25, nrow=5, ncol=5) %>%
        terra::rast()
    })
    
    observeEvent(input$get_terrain, {
      print("get_terrain button event")
      dem <- get_dem(xs$xs())
      print(dem)
      
      tmap_mode("view")   # ensure tmnap mode is view or no output is produced!
      map <- get_terrain_map(xs, dem)  +
        tm_basemap("USGS.USTopo")
      
      output$terrain_map <- renderLeaflet({
        map %>%
          tmap_leaflet(in.shiny = TRUE) %>%
          addLayersControl(
            overlayGroups = c("Elevation", "Cross Section"),
            position = "topleft")
      })
      
      output$draw_fl_button <- renderUI({
        actionButton(session$ns("draw_fl"), "Draw Flowline")
      })
    })
    
    dem <- dem
  })
}

get_dem_app <- function(input, output, session) {
  ui <- page_navbar(title = "Main App", id = "main",
    nav_panel(title = "Draw XS",
      draw_xs_ui("xs_editor"),
    )
  )
  server <- function(input, output, session) {
    draw_xs_server("xs_editor")
    get_dem_server("get_dem", output$xs)
  }
  shinyApp(ui, server)
}