#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom shinybusy add_busy_spinner spin_epic
#' @importFrom bslib page_navbar nav_panel layout_sidebar sidebar bs_theme
#' @importFrom mapedit editModUI
#' @importFrom tmap tmapOutput
#' 
#' @noRd
app_ui <- function(request) {

  tagList(
    golem_add_external_resources(),
    
    add_busy_spinner(spin = "fading-circle", 
                     #color = "#FFFFFF", margins = c(20,20),
                     position = "full-page"),
    
    page_navbar(title = "Tiered Assessment", 
      id = "main",
      theme = bs_theme(bootswatch = "cerulean", version = 5),
      
      nav_panel(title = "Draw XS",
        layout_sidebar(
          # display the xs editing module
          editModUI(id = "xs_editor"), 
          sidebar = sidebar(title = "Draw XS Instructions", 
            position = "right", width = "25%",
            uiOutput("draw_xs_instructions"),
            actionButton('get_terrain', 'Get Terrain'),
            uiOutput('view_terrain_button')
            ))),
      
      nav_panel(title = "View Terrain",
        layout_sidebar(
          # display the terrain map
          tmapOutput("terrain_map", mode = "view"),
          sidebar = sidebar(title = "View Terrain Instructions", 
            position = "right", width = "25%",
            uiOutput("view_terrain_instructions"),
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