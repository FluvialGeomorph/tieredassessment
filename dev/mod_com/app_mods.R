library(shiny)
library(bslib)

# Draw XS
draw_xs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    xs <- reactive({ data.frame(Seq = integer()) %>% 
      st_as_sf(geometry = st_sfc(), crs = 3857) })
  })
}

# Get Terrain Module
get_terrain_server <- function(id, xs, get_terrain_event) {
  moduleServer(id, function(input, output, session) {
    dem <- reactive({ raster <- matrix(1:25, nrow=5, ncol=5) %>%
        terra::rast() })
    observeEvent(get_terrain_event, {
      print("get_terrain buton event")
      return(list("dem" = dem()))
    }) 
  })
}

# App
ui <- page_navbar(title = "Main App",
                  id = "main",
                  nav_panel(title = "Draw XS", layout_sidebar(
                    "MAP",
                    sidebar = sidebar(
                      position = "right",
                      width = "25%",
                      actionButton("draw_xs", "Draw XS"),
                      actionButton("get_terrain", "Get Terrain")
                    )))
                  )
server <- function(input, output, session) {
  # Draw XS
  observeEvent(input$draw_xs, {
    print("draw_xs button event")
  })
  #return(list("xs" = xs()))
  
  #xs <- draw_xs_server("xs_editor")
  
  # Get Terrain
  
  #dem <- get_terrain_server("get_dem", xs, reactive(input$get_terrain))
  
  # Draw FL
  
}
shinyApp(ui, server)