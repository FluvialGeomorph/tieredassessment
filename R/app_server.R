#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom bslib nav_select
#' @importFrom htmltools tags
#' @importFrom purrr map
#' @importFrom leaflet leaflet addProviderTiles setView addLayersControl 
#'                     renderLeaflet leafletProxy
#' @importFrom dplyr %>% bind_rows mutate select
#' @importFrom mapedit editMod
#' @importFrom leafpm addPmToolbar pmToolbarOptions
#' @importFrom sf st_as_sf st_sfc
#' @importFrom tmap qtm tm_basemap tmap_leaflet
#' @importFrom terra plot
#' 
#' @noRd
app_server <- function(input, output, session) {
  
  xs <- draw_xs_server("xs_editor")
  
  # Define an empty flowline
  fl <- reactive({
    fl <- data.frame(ReachName = as.character()) %>%
      st_as_sf(geometry = st_sfc(),
               crs = 3857)  # ensure Web Mercator
  })
  

  # Define the terrain map
  tmap_mode("view")
  map <- qtm(fl())

  # Define the draw_fl_map
  draw_fl_map <- reactive({
    tmap_leaflet(map()) 
  })
  
  # # Define the draw_fl mapedit module
  draw_fl <- callModule(editMod,
                        id = "fl_editor",
                        leafmap = draw_fl_map,
                        targetLayerId = fl,
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
  ns <- shiny::NS("fl_editor")
  
  observeEvent(input$get_terrain, {
    # overwrite dem
    dem <- get_dem(xs)
    print(dem)
    
    # Create the terrain_map
    tmap_mode("view")   # ensure tmnap mode is view or no output is produced!
    map <- get_terrain_map(xs, dem)  +
      tm_basemap("USGS.USTopo")
    
    output$terrain_map <- renderLeaflet({
      map %>%
        tmap_leaflet(in.shiny = TRUE) %>%
        addLayersControl(
          overlayGroups = c("Elevation", "Cross Section"),
          position = "topleft")
    })
    
    # Add view terrain button
    output$view_terrain_button <- renderUI({
        actionButton("draw_flowline", "Draw Flowline")
    })
  })
  
  observeEvent(input$view_terrain, {
    nav_select(id = "main", selected = "Draw Flowline", session)
    print("Draw Flowline button")
  })
  
  observeEvent(input$calc_xs, {
    # get finished fl
    new_fl <- sf::st_transform(draw_fl()$finished, crs = 3857) # Web Mercator
    print(new_fl)
  })
  

  # Instructions
  ## create draw xs page instructions
  output$draw_xs_instructions <- renderUI({
    steps <- c('Zoom to the desired AOI.', 
               'Draw cross sections beginning with the most downstream cross section first.', 
               'Click the "Get Terrain" button below to retrieve the digital elevation model (DEM).',
               'View the terrain using the "View Terrain" button or top menu.')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
  
  ## create draw flowline page instructions
  output$draw_flowline_instructions <- renderUI({
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
