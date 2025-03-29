#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom bslib page_navbar nav_panel layout_sidebar sidebar bs_theme
#' @importFrom mapedit editModUI
#' @importFrom leaflet leafletOutput
#' 
#' @noRd
app_ui <- function(request) {

  tagList(
    golem_add_external_resources(),
    
    page_navbar(title = "Tiered Assessment", 
      id = "main",
      theme = bs_theme(bootswatch = "cerulean", version = 5),
      
      nav_panel(title = "Draw XS",
        layout_sidebar(
          # display the xs editing module
          editModUI(id = "xs_editor_ui_id"), 
          sidebar = sidebar(title = "Draw XS Instructions", 
            position = "right", width = "25%",
            uiOutput("draw_xs_instructions"),
            actionButton('get_terrain', 'Get Terrain'),
            uiOutput('draw_fl_button')
            ))),
      
      nav_panel(title = "Draw Flowline",
        layout_sidebar(
          # display fl editing module
          editModUI(id = "fl_editor_ui_id"),
          sidebar = sidebar(title = "Draw Flowline Instructions", 
            position = "right", width = "25%",
            uiOutput("draw_fl_instructions"),
            actionButton("calc_xs", "Calc XS")
            ))),
      
      nav_panel(title = "Calc XS",
        layout_sidebar(
          # map
          sidebar = sidebar(title = "Calculate XS Instructions",
            position = "right", width = "25%",
            uiOutput("calc_xs_instructions")
            # Calc results
            )))
    )
  )
}