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
#' @importFrom leaflet.extras addSearchOSM searchOptions
#' @importFrom sf st_as_sf st_sfc
#' @importFrom tmap qtm tm_basemap tmap_leaflet
#' @importFrom terra plot
#' 
#' @noRd
app_server <- function(input, output, session) {
  
  # Define an empty cross section
  xs <- reactive({
    sf <- data.frame(Seq = integer()) %>%
      st_as_sf(geometry = st_sfc(), 
               crs = 3857)  # ensure Web Mercator
    return(sf)
  })
  
  # Define an empty flowline
  fl <- reactive({
    fl <- data.frame(ReachName = as.character()) %>%
      st_as_sf(geometry = st_sfc(),
               crs = 3857)  # ensure Web Mercator
  })
  
  # Define an empty dem
  dem <- reactive({
    raster <- matrix(1:25, nrow=5, ncol=5) %>%
      terra::rast()
    return(raster)
  })
  
  # Define the draw_xs_map  
  draw_xs_map <- leaflet() %>%
    setView(lng = -93.85, lat = 37.45, zoom = 14) %>%
    addProviderTiles("USGS.USTopo", group = "USGS Topo") %>%
    addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
    leaflet.extras::addSearchOSM(
      options = searchOptions(collapsed = TRUE, 
                              autoCollapse = TRUE,
                              autoCollapseTime = 20000,
                              minLength = 3,
                              hideMarkerOnCollapse = TRUE,
                              zoom = 14)) %>%
    addLayersControl(
      baseGroups = c("USGS Topo", "Imagery"),
      position = "topleft")
    
  # Define the draw_xs mapedit module
  xs_editor_ui <- callModule(editMod,
                        id = "xs_editor_ui_id",
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
  
  
  observeEvent(input$get_terrain, {
    # get finished xs
    new_xs <- sf::st_transform(xs_editor_ui()$finished, 
                               crs = 3857) # Web Mercator
    
    xs <- shiny::req(xs()) %>%
      bind_rows(., new_xs) %>% 
      mutate(Seq = as.numeric(row.names(.))) %>%
      select(Seq, geometry) 
    # save test data
    sf::st_write(xs, file.path(golem::get_golem_wd(), 
                              "inst", "extdata", "xs.shp"), delete_dsn = TRUE)  
    print(xs)
    
    # overwrite dem
    dem <- get_dem(xs)
    print(dem)
    
    # Create the terrain_map
    tmap_mode("view")   # ensure tmnap mode is view or no output is produced!
    terrain_map <- 
      tmap_leaflet(get_terrain_map(xs, dem), in.shiny = TRUE) %>%
      addProviderTiles("USGS.USTopo", group = "USGS Topo") %>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
      addLayersControl(
        baseGroups = c("USGS Topo", "Imagery"),
        overlayGroups = c("Elevation", "Cross Section"),
        position = "topleft")
    
    # Define the draw_fl mapedit module
    fl_editor_ui <- callModule(editMod,
                               id = "fl_editor_ui_id",
                               leafmap = terrain_map,
                               targetLayerId = fl,
                               crs = 3857,
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
    # Add view terrain button
    output$draw_fl_button <- renderUI({
        actionButton("draw_flowline", "Draw Flowline")
    })
  })
  
  observeEvent(input$draw_flowline, {
    nav_select(id = "main", selected = "Draw Flowline", session)
  })
  
  observeEvent(input$calc_xs, {
    # get finished fl
    edited_fl <- fl_editor_ui()$finished
    
    new_fl <- sf::st_transform(edited_fl, 
                               crs = 3857) # Web Mercator
    # save test data
    sf::st_write(new_fl, file.path(golem::get_golem_wd(), 
                              "inst", "extdata", "fl.shp"), delete_dsn = TRUE)  
    print(fl)
    
    # fl logic here
  })
  

  # Instructions
  ## create draw xs page instructions
  output$draw_xs_instructions <- renderUI({
    steps <- c('Use the "Search" or "Zoom" tools to locate your desired area of interest (AOI).', 
               'Use the "Draw Polyline" tool to draw cross sections.', 
               'Click the "Get Terrain" button below to retrieve the digital elevation model (DEM) for your AOI.',
               'Click the "Draw Flowline" button to go to the next step.')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
  
  ## create draw flowline page instructions
  output$draw_fl_instructions <- renderUI({
    steps <- c('Use the "Draw Polyline" tool to draw the centerline of the stream in your AOI.', 
               "Use the DEM's legend to trace the lowest elevation along the stream centerline.", 
               'Click the "Calc XS" button to go to the next step.')
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
