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
#' @importFrom dplyr %>% bind_rows mutate select filter
#' @importFrom mapedit editMod
#' @importFrom leafpm addPmToolbar pmToolbarOptions
#' @importFrom leaflet.extras addSearchOSM searchOptions
#' @importFrom sf st_as_sf st_sfc
#' @importFrom tmap qtm tm_basemap tmap_leaflet
#' @importFrom terra plot crs
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom fluvgeo compare_long_profile xs_compare_plot_L1
#' @noRd
app_server <- function(input, output, session) {
  # Define reactives ##########################################################
  # Define reach name
  reach_name <- reactiveVal({
    reach_name <- NULL
  })
  # Define an empty cross section
  xs <- reactive({
    xs <- data.frame(Seq = integer()) %>%
      st_as_sf(geometry = st_sfc(), 
               crs = 3857)  # ensure Web Mercator
    return(xs)
  })
  #makeReactiveBinding("xs")       # no need, reactive created by xs_editor_ui
  # Define an empty cross section points
  xs_pts <- reactive({
    xs_pts <- data.frame(Seq = integer()) %>%
      st_as_sf(geometry = st_sfc(), 
               crs = 3857)  # ensure Web Mercator
    return(xs_pts)
  })
  makeReactiveBinding("xs_pts")
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
  # Define an empty detrend
  detrend <- reactive({
    raster <- matrix(1:25, nrow=5, ncol=5) %>%
      terra::rast()
    terra::crs(raster) <- "EPSG:3857"
    return(raster)
  })
  makeReactiveBinding("detrend")
  
  # Ensure fl_editor_ui mapedit module available at app scope
  fl_editor_ui <- NULL
  makeReactiveBinding("fl_editor_ui")

  # Draw XS ###################################################################
  # Define the leaflet draw_xs_map
  draw_xs_map <- get_leaflet(search = TRUE)

  # Define the draw_xs mapedit module
  xs_editor_ui <- callModule(editMod,
                        id = "xs_editor_ui_id",
                        leafmap = draw_xs_map,
                        targetLayerId = xs,
                        crs = 4326,    # only supports 4326, don't change
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
  
  observeEvent(xs_editor_ui()$finished, {
    # Add view terrain button
    output$draw_fl_button <- renderUI({
        actionButton("draw_flowline", "Draw Flowline")
    })
  })
  
  # Draw Flowline ###############################################################
  observeEvent(input$draw_flowline, {
    show_modal_spinner(spin = "circle", text = "Retrieving Terrain")
    # get finished xs
    xs_mapedit <- xs_editor_ui()$finished
    print("mapedit xs -------------------------------------------------------")
    #save_test_data(xs_mapedit, "xs_mapedit")
    print(xs_mapedit)
    xs_mapedit <- sf_fix_crs(xs_mapedit)
    print("tranform xs to 3857 ----------------------------------------------")
    xs_3857 <- sf::st_transform(xs_mapedit, crs = 3857) # Web Mercator
    xs <<- xs_3857 %>%
      mutate(Seq = as.numeric(row.names(.))) %>%
      select(Seq, geometry)
    #save_test_data(xs, "xs")
    print(xs)
    # Overwrite dem
    dem <<- get_dem(xs)
    print("Returned DEM -----------------------------------------------------")
    print(dem)
    # Create the leaflet terrain_map
    terrain_map <- get_terrain_leaflet(xs, dem)
    # Define the draw_fl mapedit module
    fl_editor_ui <<- callModule(editMod,
                               id = "fl_editor_ui_id",
                               leafmap = terrain_map,
                               targetLayerId = fl,
                               crs = 4326,  # only supports 4326, don't change
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
    # Navigate to Draw Flowline page
    nav_select(id = "main", selected = "Draw Flowline", session)
    remove_modal_spinner()
    
    observeEvent(fl_editor_ui()$finished, {
      # Add view terrain button
      output$view_results_button <- renderUI({
        actionButton("view_results", "View Results")
      })
    })
  })
  
  # View Results ##############################################################
  observeEvent(input$view_results, {
    show_modal_spinner(spin = "circle", text = "Calculating Geometry")
    # get finished fl
    fl_mapedit <- fl_editor_ui()$finished
    print("mapedit fl -------------------------------------------------------")
    #save_test_data(fl_mapedit, "fl_mapedit")
    print(fl_mapedit)
    fl_mapedit <- sf_fix_crs(fl_mapedit)
    fl_3857 <- sf::st_transform(fl_mapedit, crs = 3857) # Web Mercator
    print("Digitized flowline -----------------------------------------------")
    # filter for the last digitized flowline (can only have one flowline)
    fl_3857_latest <- fl_3857 %>% filter(layerId == max(layerId))
    #save_test_data(fl_3857_latest, "fl_edited")
    print(fl_3857_latest)
    # Process Flowline
    print("flowline ---------------------------------------------------------")
    print(dem)
    fl <<- flowline(fl_3857_latest, reach_name = "current stream", dem)
    #save_test_data(fl, "fl")
    print(fl)
    # Process Flowline points
    print("flowline points---------------------------------------------------")
    fl_pts <<- flowline_points(fl, dem, station_distance = 5)
    #save_test_data(fl_pts, "fl_pts")
    print(fl_pts)
    # Calculate Detrend
    print("detrend ----------------------------------------------------------")
    detrend <- dem           # bogus move until I get detrend function working
    # Process cross sections
    print("cross section ----------------------------------------------------")
    xs <<- cross_section(xs, fl_pts)
    print(xs)
    # Process Cross Section Points
    print("cross section points ---------------------------------------------")
    station_distance = 1
    xs_pts <<- cross_section_points(xs, dem, detrend, station_distance)
    print(xs_pts)
    # Create results map
    print("create results map -----------------------------------------------")
    output$results_map <- renderLeaflet({
      get_results_leaflet(fl, xs, dem)
    })
    # Render the longitudinal profile plot
    print("longitudinal profile plot ----------------------------------------")
    output$long_profile <- renderPlot({
      fl_pts_list <- list("latest" = fl_pts)
      compare_long_profile(stream = "current stream", fl_pts_list)
    })
    # Render the cross section plot
    print("cross section plot -----------------------------------------------")
    updateSelectInput(session, "pick_xs",
                      choices = seq(min(xs$Seq), max(xs$Seq))
      )
    output$xs_plot <- renderPlot({
      xs_pts_list <- list("latest" = xs_pts)
      xs_compare_plot_L1(stream = "current stream", xs_number = input$pick_xs, 
                         xs_pts_list, extent = "all")
    })
    # Navigate to the Results nav_panel
    nav_select(id = "main", selected = "Results", session)
    remove_modal_spinner()
  })
  

  # Instructions ###############################################################
  ## create draw xs page instructions
  output$draw_xs_instructions <- renderUI({
    steps <- c('Use the "Search" or "Zoom" tools to locate your desired area of interest (AOI).', 
               'Use the "Draw Polyline" tool to draw cross sections.', 
               'Click the "Draw Flowline" button to go to the next step.')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
  
  ## create draw flowline page instructions
  output$draw_fl_instructions <- renderUI({
    steps <- c('Use the "Draw Polyline" tool to draw the centerline of the stream in your AOI.', 
               "Use the DEM's color ramp to trace the lowest elevation along the stream centerline.", 
               'Click the "View Results" button to go to the next step.')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
} 
