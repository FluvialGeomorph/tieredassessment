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
  library(leafpm)
  library(sf)
  
  tagList(# Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic
    fluidPage(
      titlePanel("Shiny Leaflet Mapedit"),
      
      sidebarLayout(
        position = "right",
        sidebarPanel(actionButton('calc_xs', 'Calculate XS')),
        mainPanel(editModUI(id = "xs"))
      )
    )
  )
}