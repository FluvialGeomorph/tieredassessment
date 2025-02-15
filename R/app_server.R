#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom htmltools tags
#' @importFrom purrr map
#' @importFrom leaflet leaflet addTiles setView
#' @importFrom dplyr %>% bind_rows
#' @importFrom mapedit editMod
#' @importFrom leafpm addPmToolbar pmToolbarOptions
#' @importFrom sf st_as_sf st_sfc
#' @importFrom terra plot
#' 
#' @noRd
app_server <- function(input, output, session) {
  
  # create the cross sections
  xs <- reactive({
    sf <- data.frame(Seq = integer()) %>%
      st_as_sf(geometry = st_sfc(), 
               crs = 4326)
    return(sf)
  })
  
  # Create the draw_xs_map  
  draw_xs_map <- leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  
  # create the mapedit module
  draw_xs <- callModule(editMod,
                        id = "xs_editor",
                        leafmap = draw_xs_map,
                        targetLayerId = xs,
                        editor = "leafpm",
                        editorOptions = list(
                          toolbarOptions = pmToolbarOptions(
                                                 drawMarker = FALSE,
                                                 drawPolygon = FALSE,
                                                 drawCircle = FALSE,
                                                 drawRectangle = FALSE,
                                                 cutPolygon = FALSE,
                                                 position = "topright")
                          ))
  ns <- shiny::NS("xs_editor")
  
  
  # Events
  dem <- eventReactive(input$get_terrain, {
    # get finished xs
    xs <- bind_rows(shiny::req(xs()),
                    draw_xs()$finished)
    # get dem
    get_dem(xs)
  })
  
  # Instructions
  ## create draw xs page instructions
  output$draw_xs_instructions <- renderUI({
    steps <- c('Zoom to the desired AOI.', 
               'Draw cross sections.', 
               'Click the "Get Terrain" button below to retrieve the digital elevation model (DEM).',
               'View the terrain using the "View Terrain" item on the top menu.')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
  
  ## create view terrain page instructions
  output$view_terrain_instructions <- renderUI({
    steps <- c('Do this.', 
               'Then do that.', 
               'Then do this other important thing.',
               'Finally all your dreams will come true..')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
  
  ## create calc xs page instructions
  output$calc_xs_instructions <- renderUI({
    steps <- c('Do this.', 
               'Then do that.', 
               'Then do this other important thing.',
               'Finally all your dreams will come true..')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
  
}
