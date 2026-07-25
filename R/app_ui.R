#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @param skin Normalized application skin configuration.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom mapedit editModUI
#' @importFrom leaflet leafletOutput
#' @importFrom gt gt_output
#' @noRd
app_ui <- function(request, skin = load_app_skin()) {
  # Help text and variable values
  channel_rem_info <- "Set the channel's water surfce level in Relative Elevation Model (REM) units."
  floodplain_rem_info <- "Set the floodplain's water surface level in Relative Elevation Model (REM) units."
  discharge_info <- "This form of the *Gauckler-Manning formula* is used to calculate discharge in the tables below. "
  mannings_choices <- c(
    "(a) Clean, straight, no deep pools (n = 0.030)" = 0.030,
    "(b) Same as (a), but more stones and weeds (n = 0.035)" = 0.035,
    "(c) Clean, winding, some pools and shoals (n = 0.040)" = 0.040,
    "(d) Same as (c), but some weeds and stones (n = 0.045)" = 0.045,
    "(e) Same as (c), at lower stages, with less effective slopes and sections (n = 0.048)" = 0.048,
    "(f) Same as (d), but more stones (n = 0.050)" = 0.050,
    "(g) Sluggish reaches, weedy, deep pools (n = 0.070)" = 0.070,
    "(h) Very weedy reaches, seep pools or floodways with heavy stands of timber and underbrush (n = 0.100)" = 0.100
  )
  
  tagList(
    tags$head(
      tags$style("
        .scrollable-accordion .accordion-body {
          max-height: 300px; overflow-y: scroll; resize: vertical;
          display: flex; flex-direction: column-reverse;
        }"
      )
    ),
    golem_add_external_resources(skin),
    page_navbar(
      title = skin$identity$app_title,
      id = "main",
      # footer = accordion(
      #   id = "logs",
      #   open = FALSE,
      #   class = "scrollable-accordion",
      #   accordion_panel(title = "Console", htmlOutput("console")),
      theme = bs_theme(
        bootswatch = skin$theme$bootswatch,
        version = skin$theme$version
      ),
      
      nav_panel(
        title = skin$workflow$draw_xs$nav_label,
        value = "draw_xs",
        layout_sidebar(
        # Display the xs editing module
        editModUI(id = "xs_editor_ui_id"),
        sidebar = sidebar(
          title = skin$workflow$draw_xs$sidebar_title,
          position = "right",
          width = "25%",
          uiOutput("draw_xs_instructions"),
          uiOutput("draw_fl_button")
        )
      )),
      
      nav_panel(
        title = skin$workflow$draw_flowline$nav_label,
        value = "draw_flowline",
        layout_sidebar(
        # Display fl editing module
        editModUI(id = "fl_editor_ui_id"),
        sidebar = sidebar(
          title = skin$workflow$draw_flowline$sidebar_title,
          position = "right",
          width = "25%",
          uiOutput("draw_fl_instructions"),
          #actionButton("view_results", "View Results")
          uiOutput("view_results_button")
        )
      )),
      
      nav_panel(
        title = skin$workflow$results$nav_label,
        value = "results",
        layout_sidebar(
        # Display results_map
        leafletOutput("results_map"),
        sidebar = sidebar(
          position = "right",
          width = "50%",
          accordion(
            id = "Results",
            open = c("Cross Sections", "Discharge"),
            accordion_panel(
              title = "Longitudinal Profile", 
              plotOutput("long_profile", height = "250px")), 
            accordion_panel(
              title = "Cross Sections",
              selectInput("pick_xs", 
                          label = "Select a cross section:", choices = c(1)),
              layout_columns(
                card(
                  card_header(
                    "Set Channel REM",
                    tooltip(
                      trigger = bs_icon("info-circle"),
                      placement = "right",
                      channel_rem_info
                    )
                  ),
                  sliderInput(
                    inputId = "channel_elevation", label = NULL,
                    min = 100, max = 130, value = 103, 
                    round = -1, step = 0.1
                  )
                ),
                card(
                  card_header(
                    "Set Floodplain REM",
                    tooltip(
                      trigger = bs_icon("info-circle"),
                      placement = "right",
                      floodplain_rem_info
                    )
                  ),
                  sliderInput(
                    inputId = "floodplain_elevation", label = NULL,
                    min = 100, max = 130, value = 112, 
                    round = -1, step = 0.1
                  )
                )
              ),
              plotOutput("xs_plot_channel", height = "250px"),
              plotOutput("xs_plot_floodplain", height = "250px"),
              card(
                card_header("Storage Volume", class = "p-2"),
                card_body(class = "p-0", gt_output("floodplain_volumes"))
              )
            ),
            accordion_panel(
              title = "Discharge",
              layout_columns(
                withMathJax("$$Q = \\frac{1.486}{n} A R ^\\frac{2}{3} S^\\frac{1}{2}$$"),
                tooltip(bs_icon("info-circle"), 
                        discharge_info, placement = "auto")
              ),
              layout_columns(
                card(
                  card_header("Channel", class = "p-2"),
                  card_body(
                    class = "p-2",
                    selectInput(
                      inputId = "channel_mannings",
                      label = "Set Manning's n:",
                      choices = mannings_choices
                    ),
                    gt_output("channel_discharge")
                  )
                ),
                card(
                  card_header("Floodplain", class = "p-2"),
                  card_body(
                    class = "p-2",
                    selectInput(
                      inputId = "floodplain_mannings",
                      label = "Set Manning's n:",
                      choices = mannings_choices
                    ),
                    gt_output("floodplain_discharge")
                  )
                )
              )
            )
          )
        )
      ))
    )
  )
}
