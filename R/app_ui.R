#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom bslib page_sidebar sidebar bs_theme
#' @importFrom mapedit editModUI
#' 
#' @noRd
app_ui <- function(request) {

  tagList(# Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic
    page_sidebar(
      theme = bs_theme(bootswatch = "cerulean"),
      title = "EMRRP Tiered Assessment",
      editModUI(id = "xs_editor"), 
      sidebar = sidebar(
        position = "right",
        actionButton('calc_xs', 'Calculate XS'),
        tableOutput("xs_stats"),
        plotOutput("plot"),
        width = "25%"
      )
    )
    
  )
}