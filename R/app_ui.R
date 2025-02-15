#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom bslib page_navbar nav_panel layout_sidebar sidebar bs_theme
#' @importFrom mapedit editModUI
#' 
#' @noRd
app_ui <- function(request) {

  tagList(
    golem_add_external_resources(),
    
    page_navbar(title = "EMRRP Tiered Assessment",
      theme = bs_theme(bootswatch = "cerulean", version = 5),
      
      nav_panel(title = "Draw XS",
        layout_sidebar(
          editModUI(id = "xs_editor"), 
          sidebar = sidebar(title = "Draw XS Instructions", 
            position = "right", width = "25%",
            uiOutput("draw_xs_instructions"),
            actionButton('get_terrain', 'Get Terrain')
            ))),
      
      nav_panel(title = "View Terrain",
        layout_sidebar(
          sidebar = sidebar(title = "View Terrain Instructions", 
            position = "right", width = "25%",
            uiOutput("view_terrain_instructions"),
            actionButton("calc_xs", "Calc XS")
            ))),
      
      nav_panel(title = "Calc' XS",
        layout_sidebar(
            sidebar = sidebar(title = "Calculate XS Instructions",
            position = "right", width = "25%",
            uiOutput("calc_xs_instructions")
            )))
    )
  )
}