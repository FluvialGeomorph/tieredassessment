#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom mapedit editMod
#' @importFrom sf write_sf
#' @noRd
#' 
app_server <- function(input, output, session) {
  
  # Create the map  
  map <- leaflet() %>%
      addTiles()
  
  edits <- callModule(editMod,
                      leafmap = map,
                      id = "map",
                      editor = "leafpm")
    
  observeEvent(input$save, {
    
    geom <- edits()$finished
    
    if (!is.null(geom)) {
      
      
      # Save layer
      assign('new_geom', geom, envir = .GlobalEnv)
      sf::write_sf(geom, 'new_geom.geojson', 
                   delete_layer = TRUE, delete_dsn = TRUE)
    }
  })
}
