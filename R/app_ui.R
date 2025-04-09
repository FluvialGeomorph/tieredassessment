#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom bslib page_navbar nav_panel layout_sidebar sidebar bs_theme
#'                   accordion accordion_panel
#' @importFrom mapedit editModUI
#' @importFrom leaflet leafletOutput
#' @importFrom shinyWidgets slimSelectInput noUiSliderInput
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
          # Display the xs editing module
          editModUI(id = "xs_editor_ui_id"), 
          sidebar = sidebar(title = "Draw XS Instructions", 
            position = "right", width = "25%",
            uiOutput("draw_xs_instructions"),
            uiOutput('draw_fl_button')
          ))),
      
      nav_panel(title = "Draw Flowline",
        layout_sidebar(
          # Display fl editing module
          editModUI(id = "fl_editor_ui_id"),
          sidebar = sidebar(title = "Draw Flowline Instructions", 
            position = "right", width = "25%",
            uiOutput("draw_fl_instructions"),
            #actionButton("view_results", "View Results")
            uiOutput('view_results_button')
          ))),
      
      nav_panel(title = "Results",
        layout_sidebar(
          # Display results_map
          leafletOutput("results_map"),
          sidebar = sidebar(
            position = "right", width = "50%",
            accordion(
              id = "Results",
              open = c("Cross Sections"),
              accordion_panel(
                title = "Longitudinal Profile",
                plotOutput("long_profile")),
              accordion_panel(
                title = "Cross Sections",
                slimSelectInput(inputId = "pick_xs", 
                                label = "Select a cross section:", 
                                choices = prepare_slim_choices(
                                  xs_pts,
                                  label = Seq,
                                  value = Seq
                                )),
                numericInput("bankfull_elevation", "Select water level:",
                             value = NULL),
                # noUiSliderInput(inputId = "bankfull_elevation",
                #                 label = "Select water level:",
                #                 min = NULL, max = NULL,
                #                 value = NULL),
                plotOutput("xs_plot")
                # table of dimensions
              )
            )
          )
        )
      )
    )
  )
}