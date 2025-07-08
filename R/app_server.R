#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib nav_select
#' @importFrom htmltools tags
#' @importFrom purrr map
#' @importFrom leaflet leaflet addProviderTiles setView addLayersControl
#'                     renderLeaflet leafletProxy leafletOptions leafletCRS
#'                     removeShape addPolygons flyTo
#' @importFrom dplyr %>% bind_rows mutate select filter distinct
#' @importFrom mapedit editMod
#' @importFrom leafpm addPmToolbar pmToolbarOptions pmDrawOptions
#' @importFrom leaflet.extras addSearchOSM searchOptions
#' @importFrom sf st_as_sf st_sfc st_transform
#' @importFrom terra plot crs ifel as.polygons disagg relate vect
#' @importFrom tidyterra filter mutate
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom fluvgeo sf_fix_crs get_dem detrend water_surface_poly
#'             xs_pts_classify hydroflatten_dem floodplain_volume
#'             get_leaflet get_terrain_leaflet get_results_leaflet
#'             flowline flowline_points cross_section cross_section_points
#'             compare_long_profile xs_compare_plot_L2
#'             cross_section_dimensions_L2
#' @importFrom shinyWidgets updateAutonumericInput updateNoUiSliderInput
#' @importFrom gt render_gt
#' @noRd
app_server <- function(input, output, session) {
  # Console Logging ###########################################################
  
  # Define reactives ##########################################################
  reach_name <- reactiveVal({
    reach_name <- NULL
  })
  xs <- reactive({
    empty_sf()
  })
  #makeReactiveBinding("xs")       # no need, reactive created by xs_editor_ui
  xs_dims_l2 <- reactive({
    empty_sf()
  })
  makeReactiveBinding("xs_dims_l2")
  xs_pts <- reactive({
    empty_sf()
  })
  makeReactiveBinding("xs_pts")
  # Define an empty flowline
  fl <- reactive({
    empty_sf()
  })
  makeReactiveBinding("fl")
  fl_pts <- reactive({
    empty_sf()
  })
  makeReactiveBinding("fl_pts")
  dem <- reactive({
    empty_raster()
  })
  makeReactiveBinding("dem")
  rem <- reactive({
    empty_raster()
  })
  makeReactiveBinding("rem")
  trend <- reactive({
    empty_raster()
  })
  makeReactiveBinding("trend")
  channel_poly <- reactive({
    empty_sf()
  })
  makeReactiveBinding("channel_poly")
  floodplain_poly <- reactive({
    empty_sf()
  })
  makeReactiveBinding("floodplain_poly")
  channel_ws <- reactive({
    empty_raster()
  })
  makeReactiveBinding("channel_ws")
  floodplain_ws <- reactive({
    empty_raster()
  })
  makeReactiveBinding("floodplain_ws")
  channel_vol <- reactiveVal({
    channel_vol <- NULL
  })
  floodplain_vol <- reactiveVal({
    floodplain_vol <- NULL
  })
  
  # Ensure fl_editor_ui mapedit module available at app scope
  fl_editor_ui <- NULL
  makeReactiveBinding("fl_editor_ui")
  
  # Draw XS ###################################################################
  # Define the leaflet draw_xs_map
  draw_xs_map <- get_leaflet(search = TRUE, zoom = 4)
  
  # Define the draw_xs mapedit module
  xs_editor_ui <- callModule(
    editMod,
    id = "xs_editor_ui_id",
    leafmap = draw_xs_map,
    targetLayerId = xs,
    crs = 4326,
    # only supports 4326, don't change
    editor = "leafpm",
    editorOptions = list(
      toolbarOptions = pmToolbarOptions(
        drawMarker = FALSE,
        drawPolygon = FALSE,
        drawCircle = FALSE,
        drawRectangle = FALSE,
        cutPolygon = FALSE,
        position = "topright"
      ),
      drawOptions = pmDrawOptions(snappable = FALSE, tooltips = FALSE)
    )
  )
  
  observeEvent(xs_editor_ui()$finished, {
    withCallingHandlers({
      # Add view terrain button
      output$draw_fl_button <- renderUI({
        actionButton("draw_flowline", "Draw Flowline")
      })
      log_message("Flowline drawn.")
    }, message = function(m) {
      shinyjs::html(id = "console", add = TRUE,
                    html = paste0(m$message, "<br>"))
    })
  })
  
  # Draw Flowline #############################################################
  observeEvent(input$draw_flowline, {
    show_modal_spinner(spin = "circle", text = "Retrieving Terrain")
    
    withCallingHandlers({
      # get finished xs
      xs_mapedit <- xs_editor_ui()$finished
      log_message("mapedit xs ------------------------------------------------")
      #save_test_data(xs_mapedit, "xs_mapedit")
      log_message(xs_mapedit)
      xs_mapedit <- sf_fix_crs(xs_mapedit)
      log_message("tranform xs to 3857 ---------------------------------------")
      xs_3857 <- sf::st_transform(xs_mapedit, crs = 3857) # Web Mercator
      xs <<- xs_3857 %>%
        mutate(Seq = as.numeric(row.names(.))) %>%
        select(Seq, geometry)
      #save_test_data(xs, "xs")
      log_message(xs)
      # Overwrite dem
      dem <<- get_dem(xs)
      log_message("Returned DEM ----------------------------------------------")
      log_message(dem)
    }, message = function(m) {
      shinyjs::html(id = "console", add = TRUE,
                    html = paste0(m$message, "<br>"))
    })

    # Create the leaflet terrain_map
    terrain_map <- get_terrain_leaflet(xs, dem)
    # Define the draw_fl mapedit module
    fl_editor_ui <<- callModule(
      editMod,
      id = "fl_editor_ui_id",
      leafmap = terrain_map,
      targetLayerId = fl,
      crs = 4326,
      # only supports 4326, don't change
      editor = "leafpm",
      editorOptions = list(
        toolbarOptions = pmToolbarOptions(
          drawMarker = FALSE,
          drawPolygon = FALSE,
          drawCircle = FALSE,
          drawRectangle = FALSE,
          cutPolygon = FALSE,
          position = "topright"
        ),
        drawOptions = pmDrawOptions(snappable = FALSE, tooltips = FALSE)
      )
    )
    
    # Navigate to Draw Flowline page
    nav_select(id = "main", selected = "Draw Flowline", session)
    remove_modal_spinner()
    
    observeEvent(fl_editor_ui()$finished, {
      output$view_results_button <- renderUI({
        actionButton("view_results", "View Results")
      })
    })
  })
  
  # View Results ##############################################################
  observeEvent(input$view_results, {
    show_modal_spinner(spin = "circle", text = "Calculating Geometry")
    print("mapedit fl -------------------------------------------------------")
    fl_mapedit <- fl_editor_ui()$finished
    #save_test_data(fl_mapedit, "fl_mapedit")
    print(fl_mapedit)
    fl_mapedit <- sf_fix_crs(fl_mapedit)
    fl_3857 <- sf::st_transform(fl_mapedit, crs = 3857) # Web Mercator
    print("Digitized flowline -----------------------------------------------")
    # filter for the last digitized flowline (can only have one flowline)
    fl_3857_latest <- fl_3857 %>% filter(layerId == max(layerId))
    #save_test_data(fl_3857_latest, "fl_edited")
    print(fl_3857_latest)
    print("process flowline -------------------------------------------------")
    print(dem)
    fl <<- flowline(fl_3857_latest, reach_name = "current stream", dem)
    #save_test_data(fl, "fl")
    print(fl)
    print("process flowline points ------------------------------------------")
    fl_pts <<- flowline_points(fl, dem, station_distance = 5)
    #save_test_data(fl_pts, "fl_pts")
    print(fl_pts)
    print("calculate REM ----------------------------------------------------")
    detrend <- detrend(dem, fl, fl_pts, buffer_distance = 1000)
    rem   <<- detrend$rem
    trend <<- detrend$trend
    print(rem)
    print("create channel and floodplain polys ------------------------------")
    print(input$channel_elevation)
    channel_poly <<- water_surface_poly(
      rem = rem,
      water_surface_elevation = as.numeric(req(input$channel_elevation)),
      flowline = fl
    )
    print(channel_poly)
    print(input$floodplain_elevation)
    floodplain_poly <<- water_surface_poly(
      rem = rem,
      water_surface_elevation = as.numeric(req(input$floodplain_elevation)),
      flowline = fl
    )
    print(floodplain_poly)
    print("process cross section --------------------------------------------")
    xs <<- cross_section(xs, fl_pts)
    print(xs)
    print("process cross section points -------------------------------------")
    station_distance <- 1
    xs_pts <<- cross_section_points(xs, dem, rem, station_distance)
    print(xs_pts)
    xs_pts <<- xs_pts %>%
      mutate(POINT_M_units = "m") %>%
      mutate(dem_units = "ft") %>%
      xs_pts_classify(., channel_poly, floodplain_poly, buffer_distance = 2)
    xs_pts_list <- list("latest" = xs_pts)
    print(xs_pts)
    print("create channel water surface -------------------------------------")
    print(input$channel_elevation)
    channel_ws <<- trend + (as.numeric(input$channel_elevation) - 100)
    print(channel_ws)
    print("create floodplain water surface ----------------------------------")
    print(input$floodplain_elevation)
    floodplain_ws <<- trend + (as.numeric(input$floodplain_elevation) - 100)
    print(floodplain_ws)
    print("calculate floodplain volumes -------------------------------------")
    channel_vol <<- floodplain_volume(dem = dem, watersurface = channel_ws)
    floodplain_vol <<- floodplain_volume(dem = dem, watersurface = floodplain_ws)
    print(paste(
      "channel vol: ",
      base::round(channel_vol, 2),
      "floodplain vol: ",
      base::round(floodplain_vol, 2)
    ))
    # print("calculate L2 xs dimensions ---------------------------------------")
    # xs_dims_l2 <<- xs %>%
    #   cross_section_dimensions_L2(
    #     xs_points = xs_pts,
    #     bankfull_elevation = as.numeric(input$channel_elevation),
    #     lead_n = 1,
    #     use_smoothing = FALSE,
    #     vert_units = "ft") %>%
    #   distinct(Seq, .keep_all = TRUE)
    # print(xs_dims_l2)
    # Update selectors ########################################################
    print("pick cross section -----------------------------------------------")
    updateSelectInput(session, "pick_xs", choices = seq(min(xs_pts$Seq), max(xs_pts$Seq)))
    print(input$pick_xs)
    print("pick channel_elevation ------------------------------------------")
    print(input$channel_elevation)
    #rem_min <- 100
    rem_min <- round(min(filter(
      xs_pts, Seq == as.numeric(input$pick_xs)
    )$Detrend_DEM_Z), 1) + 0.1
    rem_min <- ifelse(rem_min > 100, rem_min, 100)
    rem_max <- round(max(filter(
      xs_pts, Seq == as.numeric(input$pick_xs)
    )$Detrend_DEM_Z), 0) - 1
    print(paste0("range = ", rem_min, " - ", rem_max))
    updateNoUiSliderInput(session,
                          inputId = "channel_elevation",
                          range = c(rem_min, rem_max))
    updateNoUiSliderInput(session,
                          inputId = "floodplain_elevation",
                          range = c(rem_min, rem_max))
    updateAutonumericInput(
      session,
      inputId = "floodplain_elevation",
      value = 112,
      options = list(
        minuimumValue = min(filter(xs_pts, Seq == req(input$pick_xs))$Detrend_DEM_Z),
        maximumValue = max(filter(xs_pts, Seq == req(input$pick_xs))$Detrend_DEM_Z),
        wheelStep = 0.5
      )
    )
    # Create outputs ##########################################################
    print("create results map -----------------------------------------------")
    output$results_map <- renderLeaflet({
      get_results_leaflet(fl, xs, dem, channel_poly, floodplain_poly)
    })
    print("longitudinal profile plot ----------------------------------------")
    output$long_profile <- renderPlot({
      fl_pts_list <- list("latest" = fl_pts)
      compare_long_profile(stream = "current stream", fl_pts_list)
    })
    print("create cross section plots ---------------------------------------")
    output$xs_plot_floodplain <- renderPlot({
      xs_compare_plot_L2(
        stream = "current stream",
        xs_number = req(input$pick_xs),
        bankfull_elevation = req(input$channel_elevation),
        xs_pts_list,
        extent = "floodplain",
        aspect_ratio = NULL
      )
    })
    output$xs_plot_channel <- renderPlot({
      xs_compare_plot_L2(
        stream = "current stream",
        xs_number = req(input$pick_xs),
        bankfull_elevation = req(input$channel_elevation),
        xs_pts_list,
        extent = "channel",
        aspect_ratio = NULL
      )
    })
    print("calculate volumes ------------------------------------------------")
    output$floodplain_volumes <- render_gt(floodplain_vol_table(channel_vol, floodplain_vol))
    print("calculate discharge ----------------------------------------------")
    output$channel_discharge <- render_gt(
      xs_discharge_table(
        xs_pts = xs_pts,
        xs_number = req(input$pick_xs),
        bf_estimate = req(input$channel_elevation),
        mannings_n = as.numeric(input$channel_mannings)
      )
    )
    output$floodplain_discharge <- render_gt(
      xs_discharge_table(
        xs_pts = xs_pts,
        xs_number = req(input$pick_xs),
        bf_estimate = req(input$floodplain_elevation),
        mannings_n = as.numeric(input$floodplain_mannings)
      )
    )
    remove_modal_spinner()
    
    
    # Channel Slider ##########################################################
    observeEvent(input$channel_elevation, {
      show_modal_spinner(spin = "circle", text = "Re-calculating Geometry")
      print("update channel_elevation ---------------------------------------")
      print(req(input$channel_elevation))
      channel_poly <<- water_surface_poly(
        rem = rem,
        water_surface_elevation = as.numeric(req(input$channel_elevation)),
        flowline = fl
      )
      print(channel_poly)
      print("update cross section points classify ---------------------------")
      xs_pts <<- xs_pts_classify(xs_pts, channel_poly, floodplain_poly, buffer_distance = 2)
      xs_pts_list <- list("latest" = xs_pts)
      print("create channel water surface -----------------------------------")
      print(input$channel_elevation)
      channel_ws <<- trend + (as.numeric(input$channel_elevation) - 100)
      print(channel_ws)
      print("calculate floodplain volumes -----------------------------------")
      channel_vol <<- floodplain_volume(dem = dem, watersurface = channel_ws)
      print(paste(
        "channel vol: ",
        base::round(channel_vol, 2),
        "floodplain vol: ",
        base::round(floodplain_vol, 2)
      ))
      # print("calculate L2 xs dimensions -------------------------------------")
      # xs_dims_l2 <<- xs %>%
      #   cross_section_dimensions_L2(
      #     xs_points = xs_pts,
      #     bankfull_elevation = as.numeric(input$channel_elevation),
      #     lead_n = 1,
      #     use_smoothing = FALSE,
      #     vert_units = "ft") %>%
      #   distinct(Seq, .keep_all = TRUE)
      # print(xs_dims_l2)
      print("update results_map ---------------------------------------------")
      leafletProxy(mapId = "results_map", data = channel_poly) %>%
        flyTo(
          lng  = input$results_map_center$lng,
          lat  = input$results_map_center$lat,
          zoom = input$results_map_zoom
        ) %>%
        removeShape(layerId = "channel_poly") %>%
        addPolygons(
          data = st_transform(channel_poly, crs = 4326),
          layerId = "channel_poly",
          color = "navy",
          weight = 1,
          group = "Channel"
        )
      print("update cross section plots -------------------------------------")
      output$xs_plot_floodplain <- renderPlot({
        xs_compare_plot_L2(
          stream = "current stream",
          xs_number = req(input$pick_xs),
          bankfull_elevation = req(input$channel_elevation),
          xs_pts_list,
          extent = "floodplain",
          aspect_ratio = NULL
        )
      })
      output$xs_plot_channel <- renderPlot({
        xs_compare_plot_L2(
          stream = "current stream",
          xs_number = req(input$pick_xs),
          bankfull_elevation = req(input$channel_elevation),
          xs_pts_list,
          extent = "channel",
          aspect_ratio = NULL
        )
      })
      print("update discharge -----------------------------------------------")
      output$channel_discharge <- render_gt(
        xs_discharge_table(
          xs_pts = xs_pts,
          xs_number = req(input$pick_xs),
          bf_estimate = req(input$channel_elevation),
          mannings_n = as.numeric(input$channel_mannings)
        )
      )
      output$floodplain_volumes <- render_gt(floodplain_vol_table(channel_vol, floodplain_vol))
      remove_modal_spinner()
    })
    
    # Floodplain Slider #######################################################
    observeEvent(input$floodplain_elevation, {
      show_modal_spinner(spin = "circle", text = "Re-calculating Geometry")
      print("update floodplain_elevation ------------------------------------")
      print(req(input$floodplain_elevation))
      floodplain_poly <<- water_surface_poly(
        rem = rem,
        water_surface_elevation = as.numeric(req(input$floodplain_elevation)),
        flowline = fl
      )
      print(floodplain_poly)
      xs_pts <<- xs_pts_classify(xs_pts, channel_poly, floodplain_poly, buffer_distance = 2)
      xs_pts_list <- list("latest" = xs_pts)
      print("create floodplain water surface --------------------------------")
      print(input$floodplain_elevation)
      floodplain_ws <<- trend + (as.numeric(input$floodplain_elevation) - 100)
      print(floodplain_ws)
      print("calculate floodplain volumes -----------------------------------")
      floodplain_vol <<- floodplain_volume(dem = dem, watersurface = floodplain_ws)
      print(paste(
        "channel vol: ",
        base::round(channel_vol, 2),
        "floodplain vol: ",
        base::round(floodplain_vol, 2)
      ))
      print("update results_map ---------------------------------------------")
      leafletProxy(mapId = "results_map", data = floodplain_poly) %>%
        flyTo(
          lng  = input$results_map_center$lng,
          lat  = input$results_map_center$lat,
          zoom = input$results_map_zoom
        ) %>%
        removeShape(layerId = "floodplain_poly") %>%
        addPolygons(
          data = st_transform(floodplain_poly, crs = 4326),
          layerId = "floodplain_poly",
          color = "forestgreen",
          weight = 1,
          group = "Floodplain"
        )
      print("update cross section plots -------------------------------------")
      output$xs_plot_floodplain <- renderPlot({
        xs_compare_plot_L2(
          stream = "current stream",
          xs_number = req(input$pick_xs),
          bankfull_elevation = req(input$channel_elevation),
          xs_pts_list,
          extent = "floodplain",
          aspect_ratio = NULL
        )
      })
      output$xs_plot_channel <- renderPlot({
        xs_compare_plot_L2(
          stream = "current stream",
          xs_number = req(input$pick_xs),
          bankfull_elevation = req(input$channel_elevation),
          xs_pts_list,
          extent = "channel",
          aspect_ratio = NULL
        )
      })
      print("update discharge -----------------------------------------------")
      output$floodplain_discharge <- render_gt(
        xs_discharge_table(
          xs_pts = xs_pts,
          xs_number = req(input$pick_xs),
          bf_estimate = req(input$floodplain_elevation),
          mannings_n = as.numeric(input$floodplain_mannings)
        )
      )
      output$floodplain_volumes <- render_gt(floodplain_vol_table(channel_vol, floodplain_vol))
      remove_modal_spinner()
    })
    
    # Manning's n update ######################################################
    observeEvent(input$channel_mannings, {
      show_modal_spinner(spin = "circle", text = "Re-calculating Discharge")
      print("update discharge -----------------------------------------------")
      output$channel_discharge <- render_gt(
        xs_discharge_table(
          xs_pts = xs_pts,
          xs_number = req(input$pick_xs),
          bf_estimate = req(input$channel_elevation),
          mannings_n = as.numeric(input$channel_mannings)
        )
      )
      remove_modal_spinner()
    })
    observeEvent(input$floodplain_mannings, {
      show_modal_spinner(spin = "circle", text = "Re-calculating Discharge")
      print("update discharge -----------------------------------------------")
      output$floodplain_discharge <- render_gt(
        xs_discharge_table(
          xs_pts = xs_pts,
          xs_number = req(input$pick_xs),
          bf_estimate = req(input$floodplain_elevation),
          mannings_n = as.numeric(input$floodplain_mannings)
        )
      )
      remove_modal_spinner()
    })
    
    nav_select(id = "main", selected = "Results", session)
  }) # End View Results #######################################################
  
  
  # Instructions ##############################################################
  ## create draw xs page instructions
  output$draw_xs_instructions <- renderUI({
    steps <- c(
      'Use the "Search" or "Zoom" tools to locate your desired area of interest (AOI).',
      'Use the "Draw Polyline" tool to draw cross sections.',
      'Click the "Draw Flowline" button to go to the next step.'
    )
    ul <- htmltools::tags$ul(purrr::map(steps, function(.x) {
      tags$li(.x)
    }))
  })
  
  ## create draw flowline page instructions
  output$draw_fl_instructions <- renderUI({
    steps <- c(
      'Use the "Draw Polyline" tool to draw the centerline of the stream in your AOI.',
      "Use the DEM's color ramp to trace the lowest elevation along the stream centerline.",
      'Click the "View Results" button to go to the next step.'
    )
    ul <- htmltools::tags$ul(purrr::map(steps, function(.x) {
      tags$li(.x)
    }))
  })
}
