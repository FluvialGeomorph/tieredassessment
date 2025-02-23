#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom bslib nav_select
#' @importFrom htmltools tags
#' @importFrom purrr map
#' @importFrom leaflet leaflet addProviderTiles setView
#' @importFrom dplyr %>% bind_rows
#' @importFrom mapedit editMod
#' @importFrom leafpm addPmToolbar pmToolbarOptions
#' @importFrom sf st_as_sf st_sfc
#' @importFrom tmap renderTmap tm_basemap
#' @importFrom terra plot
#' 
#' @noRd
app_server <- function(input, output, session) {
  
  # Define the cross sections
  xs <- reactive({
    sf <- data.frame(Seq = integer()) %>%
      st_as_sf(geometry = st_sfc(), 
               crs = 3857)  # ensure Web Mercator
    return(sf)
  })
  
  # Define an empty dem
  dem <- reactive({
    raster <- matrix(1:25, nrow=5, ncol=5) %>%
      terra::rast()
    return(raster)
  })
  
  # Define the draw_xs_map  
  draw_xs_map <- leaflet() %>%
    setView(lng = -93.85, lat = 37.45, zoom = 13) %>%
    addProviderTiles("Esri.WorldTopoMap")
  
  # Define the mapedit module
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
  
  observeEvent(input$get_terrain, {
    # get finished xs
    xs <- dplyr::bind_rows(shiny::req(xs()),
                           sf::st_transform(draw_xs()$finished,
                                            crs = 3857))  # ensure Web Mercator
    print(xs)
    
    # overwrite dem
    dem <- get_dem(xs)
    print(dem)
    
    # Create the terrain_map
    tmap_mode("view")   # ensure tmnap mode is view or no output is produced!
    output$terrain_map <- renderTmap({
      get_terrain_map(xs, dem)  +
      tm_basemap("Esri.WorldTopoMap")
    })
    
    # Add view terrain button
    output$view_terrain_button <- renderUI({
        actionButton("view_terrain", "View Terrain")
    })
  })
  
  observeEvent(input$view_terrain, {
    nav_select(id = "main", selected = "View Terrain", session)
    print("View Terrain button")
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
