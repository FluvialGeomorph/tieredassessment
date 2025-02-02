#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom bslib page_sidebar sidebar
#' @importFrom mapedit editModUI
#' 
#' @noRd
app_ui <- function(request) {

  tagList(# Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic
    page_sidebar(
      title = "Shiny Leaflet Mapedit",
      editModUI(id = "xs"), 
      
      sidebar = sidebar(
        position = "right",
        actionButton('calc_xs', 'Calculate XS')
      ),
    )
    
  )
}