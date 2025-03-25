library(shiny)
library(bslib)

# Button Module
button_ui <- function(id, label) {
  actionButton(NS(id, "btn"), label = label)
}
button_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$btn)
  })
}

# Draw XS Module
draw_xs_ui <- function(id) {
  tagList(
    layout_sidebar(
      "MAP",
      sidebar = sidebar(position = "right", width = "25%",
        button_ui("get_terrain", "Get Terrain")
      )))
}
draw_xs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    xs <- reactive({ data.frame(Seq = integer()) %>% 
      st_as_sf(geometry = st_sfc(), crs = 3857)  })
    observeEvent(input$draw_xs, {
      print("draw_xs button event")
    })
    return(list("xs" = xs()))
  })
}

# Get Terrain Module
get_terrain_server <- function(id, xs, button) {
  moduleServer(id, function(input, output, session) {
    observeEvent(button(), {
      dem <- reactive({ raster <- matrix(1:25, nrow=5, ncol=5) %>%
        terra::rast() })
    })
  })
}

# App
ui <-page_navbar(title = "Main App", id = "main",
                 nav_panel(title = "Draw XS",
                           draw_xs_ui("xs_editor")))
server <- function(input, output, session) {
  xs <- draw_xs_server("xs_editor")
  dem <- get_terrain_server("get_dem", button_server("get_terrain"))
}
shinyApp(ui, server)