#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom mapedit editMod
#' @importFrom leafpm addPmToolbar pmToolbarOptions
#' @importFrom sf write_sf
#' @noRd
#' 
app_server <- function(input, output, session) {
  
  # Create the map  
  map <- leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  
  draw_xs <- callModule(editMod,
                        id = "xs",
                        leafmap = map,
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
  
  observeEvent(input$calc_xs, {
    xs <- draw_xs()$finished
    assign('xs', xs, envir = .GlobalEnv)
    sf::write_sf(xs, 'xs.geojson', delete_layer = TRUE, delete_dsn = TRUE)
    
  })
}
