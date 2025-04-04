#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom bslib nav_select
#' @importFrom htmltools tags
#' @importFrom purrr map
#' @importFrom leaflet leaflet addProviderTiles setView addLayersControl 
#'                     renderLeaflet leafletProxy leafletOptions leafletCRS
#' @importFrom dplyr %>% bind_rows mutate select
#' @importFrom mapedit editMod
#' @importFrom leafpm addPmToolbar pmToolbarOptions
#' @importFrom leaflet.extras addSearchOSM searchOptions
#' @importFrom sf st_as_sf st_sfc
#' @importFrom tmap qtm tm_basemap tmap_leaflet
#' @importFrom terra plot crs
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @noRd
app_server <- function(input, output, session) {
  # Create Reactives ##########################################################
  # Define an empty cross section
  xs <- reactive({
    xs <- data.frame(Seq = integer()) %>%
      st_as_sf(geometry = st_sfc(), 
               crs = 3857)  # ensure Web Mercator
    return(xs)
  })
  #makeReactiveBinding("xs")
  # Define an empty flowline
  fl <- reactive({
    fl <- data.frame(ReachName = as.character()) %>%
      st_as_sf(geometry = st_sfc(),
               crs = 3857)  # ensure Web Mercator
    return(fl)
  })
  makeReactiveBinding("fl")
  # Define an empty flowline
  fl_pts <- reactive({
    fl_pts <- data.frame(ReachName = as.character()) %>%
      st_as_sf(geometry = st_sfc(),
               crs = 3857)  # ensure Web Mercator
    return(fl_pts)
  })
  makeReactiveBinding("fl_pts")
  # Define an empty dem
  dem <- reactive({
    raster <- matrix(1:25, nrow=5, ncol=5) %>%
      terra::rast()
    terra::crs(raster) <- "EPSG:3857"
    return(raster)
  })
  makeReactiveBinding("dem")

  # Draw XS ###################################################################
  # Define the leaflet draw_xs_map
  draw_xs_map <- get_leaflet(search = TRUE)

  # Define the draw_xs mapedit module
  xs_editor_ui <- callModule(editMod,
                        id = "xs_editor_ui_id",
                        leafmap = draw_xs_map,
                        targetLayerId = xs,
                        crs = 4326,    # only suuports 4326, don't change
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
  
  # Get Terrain ###############################################################

  # Ensure flowline mapedit module is available at app scope
  terrain_map <- NULL
  makeReactiveBinding("terrain_map")
  fl_editor_ui <- NULL
  makeReactiveBinding("fl_editor_ui")
  
  observeEvent(input$get_terrain, {
    show_modal_spinner(spin = "circle", text = "Retrieving Terrain")
    # get finished xs
    xs_mapedit <- xs_editor_ui()$finished
    print("mapedit xs -------------------------------------------------------")
    print(xs_mapedit)
    xs_mapedit <- sf_fix_crs(xs_mapedit)
    print("tranform xs to 3857 ----------------------------------------------")
    xs_3857 <- sf::st_transform(xs_mapedit, crs = 3857) # Web Mercator
    xs <- xs() %>%
      bind_rows(., xs_3857) %>%
      mutate(Seq = as.numeric(row.names(.))) %>%
      select(Seq, geometry)
    # save test data
    # sf::st_write(xs, file.path(golem::get_golem_wd(),
    #                           "inst", "extdata", "xs_edited.shp"), 
    #              delete_dsn = TRUE)
    print("cross section transformed to 3857---------------------------------")
    print(xs_3857)
    check_crs_3857(xs_3857)
    
    # Overwrite dem
    dem <<- get_dem(xs_3857)
    print("Returned DEM -----------------------------------------------------")
    print(dem)
    check_crs_3857(dem)
    
    # Create the leaflet terrain_map
    terrain_map <- get_terrain_leaflet(xs_3857, dem)
    
    # Define the draw_fl mapedit module
    fl_editor_ui <<- callModule(editMod,
                               id = "fl_editor_ui_id",
                               leafmap = terrain_map,
                               targetLayerId = fl,
                               crs = 4326,  # only suuports 4326, don't change
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
    remove_modal_spinner()
  })
  
  # Draw Flowline ##############################################################
  observeEvent(input$draw_flowline, {
    nav_select(id = "main", selected = "Draw Flowline", session)
  })
  
  observeEvent(input$calc_xs, {
    # get finished fl
    fl_mapedit <- fl_editor_ui()$finished
    print("mapedit fl -------------------------------------------------------")
    print(fl_mapedit)
    fl_mapedit <- sf_fix_crs(fl_mapedit)
    fl_3857 <- sf::st_transform(fl_mapedit, crs = 3857) # Web Mercator
    print("Digitized flowline -----------------------------------------------")
    print(fl_3857)
    check_crs_3857(fl_3857)
    # save test data
    # sf::st_write(fl_3857, file.path(golem::get_golem_wd(),
    #                           "inst", "extdata", "fl_edited.shp"), 
    #              delete_dsn = TRUE)

    # Flowline
    print(dem)
    fl <<- flowline(fl_3857, dem)
    print("flowline ---------------------------------------------------------")
    print(fl)
    fl_pts <<- flowline_points(fl, dem, station_distance = 100)
    print("flowline points---------------------------------------------------")
    print(fl_pts)
  })
  

  # Instructions ###############################################################
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
