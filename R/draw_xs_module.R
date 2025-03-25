#' @title Draw XS UI Function
#' @param id 
#' @return tagList containing a bslib sidebar layout
#' @export
#' @import shiny
#' @importFrom bslib layout_sidebar sidebar
#' @importFrom mapedit editModUI
draw_xs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      mapedit::editModUI(NS(id, "xs_editor")),
      sidebar = sidebar(
        title = "Draw XS Instructions",
        position = "right", width = "25%",
        uiOutput(ns("draw_xs_instructions")),
        button_ui(ns("get_terrain"), "Get Terrain"),
        uiOutput(ns("draw_fl_button"))
      )
    )
  )
}

#' @title Draw XS Server Function
#' @param id 
#' @return cross section reactive
#' @export
#' @import shiny
#' @importFrom purrr map
#' @importFrom leaflet leaflet setView addProviderTiles
#' @importFrom sf st_as_sf st_sfc
#' @importFrom mapedit editMod
#' @importFrom leafpm pmToolbarOptions
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
        st_as_sf(geometry = st_sfc(), crs = 3857)  # ensure Web Mercator
    })
    
    draw_xs_map <- leaflet() %>%  # defaults to Web Mercator
      setView(lng = -93.85, lat = 37.45, zoom = 13) %>%
      addProviderTiles("USGS.USTopo")
    
    draw_xs <- callModule(
      mapedit::editMod,
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
      xs <- draw_xs()$finished
      print(xs)
      output$get_terrain_button <- renderUI({
        actionButton(NS(id, "get_terrain"), "label")
      })
      output$xs <- reactive({xs()})
    })
  })
}

draw_xs_app <- function() {
  ui <- page_navbar(title = "Main App", id = "main",
    nav_panel(title = "Draw XS",
      draw_xs_ui("xs_editor"),
    )
  )
  server <- function(input, output, session) {
    draw_xs_server("xs_editor")
  }
  shinyApp(ui, server)
}