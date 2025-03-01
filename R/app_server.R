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
    setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
    addProviderTiles("USGS.USTopo") %>%
    leaflet.extras::addSearchOSM(
      options = searchOptions(collapsed = TRUE, 
                              autoCollapse = TRUE,
                              minLength = 3,
                              hideMarkerOnCollapse = TRUE,
                              zoom = 15))
    
  # Define the draw_xs mapedit module
  xs_editor_ui <- callModule(editMod,
                        id = "xs_editor_ui",
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
    new_xs <- sf::st_transform(xs_editor_ui()$finished, 
                               crs = 3857) # Web Mercator
    
    xs <- shiny::req(xs()) %>%
      bind_rows(., new_xs) %>% 
      mutate(Seq = as.numeric(row.names(.))) %>%
      select(Seq, geometry) 
    # save test data
    # sf::st_write(xs, file.path(golem::get_golem_wd(), 
    #                            "inst", "extdata", "xs.shp"))  
    print(xs)
    
    # overwrite dem
    dem <- get_dem(xs)
    print(dem)
    
    # Create the terrain_map
    tmap_mode("view")   # ensure tmnap mode is view or no output is produced!
    terrain_map <- 
      tmap_leaflet(get_terrain_map(xs, dem) + 
                   tm_basemap("USGS.USTopo"),
                   in.shiny = TRUE) %>%
      addLayersControl(
        overlayGroups = c("Elevation", "Cross Section"),
        position = "topleft")
    print(class(terrain_map))
    
    output$terrain_map <- renderLeaflet({
      terrain_map
    })
 
    # Define the draw_fl mapedit module
    fl_editor_ui <- callModule(editMod,
                               id = "fl_editor",
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
    ns <- shiny::NS("fl_editor")
    print(ns("fl_editor_ui"))
    
    output$fl_editor_ui <- fl_editor_ui
    
    # Add view terrain button
    output$draw_fl_button <- renderUI({
        actionButton("draw_flowline", "Draw Flowline")
    })
  })
  
  observeEvent(input$draw_flowline, {
    nav_select(id = "main", selected = "Draw Flowline", session)
    print("Draw Flowline button event")
  })
  
  observeEvent(input$calc_xs, {
    # get finished fl
    new_fl <- sf::st_transform(fl_editor_ui()$finished, 
                               crs = 3857) # Web Mercator
    print(new_fl)
    
    # fl logic here
  })
  

  # Instructions
  ## create draw xs page instructions
  output$draw_xs_instructions <- renderUI({
    steps <- c('Zoom to the desired AOI.', 
               'Draw cross sections.', 
               'Click the "Get Terrain" button below to retrieve the digital elevation model (DEM).',
               'View the terrain using the "View Terrain" button or top menu.')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
  
  ## create draw flowline page instructions
  output$draw_fl_instructions <- renderUI({
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
