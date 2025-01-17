#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    map <- leaflet() %>%
      addTiles()
  
    edits <- callModule(editMod,
                     leafmap = map,
                     id = "map")
    
    observeEvent(input$save, {
        
        geom <- edits()$finished
        
        if (!is.null(geom)) {
            assign('new_geom', geom, envir = .GlobalEnv)
            sf::write_sf(geom, 'new_geom.geojson', 
                         delete_layer = TRUE, delete_dsn = TRUE)
        }
        
    })
}
