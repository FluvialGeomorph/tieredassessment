#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @param skin Normalized application skin configuration.
#'
#' @importFrom golem add_resource_path activate_js bundle_resources
#' @importFrom shinyjs useShinyjs
#' @noRd
golem_add_external_resources <- function(skin = load_app_skin()) {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    tags$link(rel = "icon", href = skin$identity$favicon),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = skin$identity$browser_title
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
    useShinyjs()
  )
}
