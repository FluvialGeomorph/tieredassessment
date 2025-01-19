#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom mapedit editModUI
#' @noRd
#' 
app_ui <- function(request) {
  library(shiny)
  library(leaflet)
  library(mapedit)
  library(sf)
  
  tagList(# Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic
    fluidPage(
      titlePanel("Minimal Shiny Leaflet Mapedit"),
      
      sidebarLayout(
        position = "right",
        
        sidebarPanel(actionButton('save', 'Save from Map')),
        
        mainPanel(editModUI("map"))
      )
    ))
}