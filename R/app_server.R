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
#' @importFrom gt render_gt
#' @noRd
app_server <- function(input, output, session) {
  # Define reactives ##########################################################
  results_loaded <- reactiveVal(FALSE)
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
    # Add view terrain button
    output$draw_fl_button <- renderUI({
      actionButton("draw_flowline", "Draw Flowline")
    })
    log_message("Cross section drawn.")
  })

  # Draw Flowline #############################################################
  observeEvent(input$draw_flowline, {
    show_modal_spinner(spin = "circle", text = "Retrieving Terrain")
    # get finished xs
    xs_mapedit <- xs_editor_ui()$finished
    log_message("mapedit xs -----------------------------------------------")
    #save_test_data(xs_mapedit, "xs_mapedit")
    log_message(xs_mapedit)
    xs_mapedit <- sf_fix_crs(xs_mapedit)
    log_message("tranform xs to 3857 --------------------------------------")
    xs_3857 <- sf::st_transform(xs_mapedit, crs = 3857) # Web Mercator
    xs <<- xs_3857 %>%
      mutate(Seq = as.numeric(row.names(.))) %>%
      select(Seq, geometry)
    #save_test_data(xs, "xs")
    log_message(xs)
    # Overwrite dem
    dem <<- get_dem(xs)
    log_message("Returned DEM ---------------------------------------------")
    log_message(dem)

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

  observeEvent(input$view_results, {
    show_modal_spinner(spin = "circle", text = "Calculating Geometry")
    on.exit(
      {
        try(remove_modal_spinner(), silent = TRUE)
      },
      add = TRUE
    )

    tryCatch(
      {
        log_message(
          "mapedit fl -----------------------------------------------"
        )
        fl_mapedit <- fl_editor_ui()$finished
        log_message(fl_mapedit)
        fl_mapedit <- sf_fix_crs(fl_mapedit)
        fl_3857 <- sf::st_transform(fl_mapedit, crs = 3857) # Web Mercator

        log_message(
          "Digitized flowline ---------------------------------------"
        )
        fl_3857_latest <- fl_3857 %>% filter(layerId == max(layerId))
        log_message(fl_3857_latest)

        log_message(
          "process flowline -----------------------------------------"
        )
        log_message(dem)
        fl <<- flowline(fl_3857_latest, reach_name = "current stream", dem)
        log_message(fl)

        log_message(
          "process flowline points -----_----------------------------"
        )
        fl_pts <<- flowline_points(fl, dem, station_distance = 5)
        log_message(fl_pts)

        log_message(
          "calculate REM --------------------------------------------"
        )
        detrend <- detrend(dem, fl, fl_pts, buffer_distance = 1000)
        rem <<- detrend$rem
        trend <<- detrend$trend
        log_message(rem)

        log_message(
          "create channel and floodplain polys ----------------------"
        )
        channel_poly <<- water_surface_poly(
          rem = rem,
          water_surface_elevation = as.numeric(isolate(
            input$channel_elevation
          )),
          flowline = fl
        )
        floodplain_poly <<- water_surface_poly(
          rem = rem,
          water_surface_elevation = as.numeric(isolate(
            input$floodplain_elevation
          )),
          flowline = fl
        )
        log_message(channel_poly)
        log_message(floodplain_poly)

        log_message(
          "process cross section ------------------------------------"
        )
        xs <<- cross_section(xs, fl_pts)
        log_message(xs)

        log_message(
          "process cross section points -----------------------------"
        )
        station_distance <- 1
        xs_pts <<- cross_section_points(xs, dem, rem, station_distance)
        xs_pts <<- xs_pts %>%
          mutate(POINT_M_units = "m") %>%
          mutate(dem_units = "ft") %>%
          xs_pts_classify(., channel_poly, floodplain_poly, buffer_distance = 2)
        xs_pts_list <- list("latest" = xs_pts)
        log_message(xs_pts)

        log_message(
          "create channel water surface -----------------------------"
        )
        channel_ws <<- trend +
          (as.numeric(isolate(input$channel_elevation)) - 100)
        log_message(channel_ws)

        log_message(
          "create floodplain water surface --------------------------"
        )
        floodplain_ws <<- trend +
          (as.numeric(isolate(input$floodplain_elevation)) - 100)
        log_message(floodplain_ws)

        log_message(
          "calculate floodplain volumes -----------------------------"
        )
        channel_vol <<- floodplain_volume(dem = dem, watersurface = channel_ws)
        floodplain_vol <<- floodplain_volume(
          dem = dem,
          watersurface = floodplain_ws
        )

        log_message(paste(
          "channel vol: ",
          base::round(channel_vol, 2),
          "floodplain vol: ",
          base::round(floodplain_vol, 2)
        ))

        log_message(
          "create results map ---------------------------------------"
        )
        output$results_map <- renderLeaflet({
          get_results_leaflet(fl, xs, dem, channel_poly, floodplain_poly)
        })

        log_message(
          "longitudinal profile plot --------------------------------"
        )
        output$long_profile <- renderPlot({
          fl_pts_list <- list("latest" = fl_pts)
          compare_long_profile(stream = "current stream", fl_pts_list)
        })

        log_message(
          "create cross section plots -------------------------------"
        )
        output$xs_plot_floodplain <- renderPlot({
          req(results_loaded())
          req(is.numeric(input$channel_elevation))
          req(length(input$channel_elevation) > 0)
          req(!is.na(input$channel_elevation))

          xs_compare_plot_L2(
            stream = "current stream",
            xs_number = input$pick_xs,
            bankfull_elevation = input$channel_elevation,
            xs_pts_list,
            extent = "floodplain",
            aspect_ratio = NULL
          )
        })

        output$xs_plot_channel <- renderPlot({
          req(results_loaded())
          req(is.numeric(input$channel_elevation))
          req(length(input$channel_elevation) > 0)
          req(!is.na(input$channel_elevation))

          xs_compare_plot_L2(
            stream = "current stream",
            xs_number = input$pick_xs,
            bankfull_elevation = input$channel_elevation,
            xs_pts_list,
            extent = "channel",
            aspect_ratio = NULL
          )
        })

        log_message(
          "calculate volumes ----------------------------------------"
        )
        output$floodplain_volumes <- render_gt(
          floodplain_vol_table(channel_vol, floodplain_vol)
        )

        log_message(
          "calculate discharge --------------------------------------"
        )
        output$channel_discharge <- render_gt({
          req(results_loaded())
          req(is.numeric(input$channel_elevation))
          req(length(input$channel_elevation) > 0)
          req(!is.na(input$channel_elevation))

          xs_discharge_table(
            xs_pts = xs_pts,
            xs_number = input$pick_xs,
            bf_estimate = input$channel_elevation,
            mannings_n = as.numeric(input$channel_mannings)
          )
        })

        output$floodplain_discharge <- render_gt({
          req(results_loaded())
          req(is.numeric(input$floodplain_elevation))
          req(length(input$floodplain_elevation) > 0)
          req(!is.na(input$floodplain_elevation))

          xs_discharge_table(
            xs_pts = xs_pts,
            xs_number = input$pick_xs,
            bf_estimate = input$floodplain_elevation,
            mannings_n = as.numeric(input$floodplain_mannings)
          )
        })

        log_message("pick cross section -------------------------------------")
        log_message(input$pick_xs)

        transition_state <- run_results_workflow_transition(
          session = session,
          input = input,
          xs_pts = xs_pts,
          results_loaded = results_loaded
        )

        log_message(paste0(
          "transition complete; pick_xs = ",
          transition_state$pick_xs
        ))

        nav_select(id = "main", selected = "Results", session)
        remove_modal_spinner()
      },
      error = function(e) {
        log_message(paste("ERROR in view_results:", conditionMessage(e)))
        log_message(paste("ERROR call:", deparse(conditionCall(e))))
        log_message(paste(capture.output(str(e)), collapse = "\n"))
        stop(e)
      }
    )
  }) ## End View Results #################################

  observeEvent(input$channel_elevation, {
    req(results_loaded())
    log_message(paste("Channel elevation value:", input$channel_elevation))
    log_message("update channel_elevation -------------------------------")
    channel_poly <<- water_surface_poly(
      rem = rem,
      water_surface_elevation = as.numeric(input$channel_elevation),
      flowline = fl
    )
    log_message(channel_poly)
    log_message("update cross section points classify -------------------")
    xs_pts <<- xs_pts_classify(
      xs_pts,
      channel_poly,
      floodplain_poly,
      buffer_distance = 2
    )
    xs_pts_list <- list("latest" = xs_pts)
    log_message("create channel water surface ---------------------------")
    log_message(input$channel_elevation)
    channel_ws <<- trend + (as.numeric(input$channel_elevation) - 100)
    log_message(channel_ws)
    log_message("calculate floodplain volumes ---------------------------")
    channel_vol <<- floodplain_volume(dem = dem, watersurface = channel_ws)
    log_message(paste(
      "channel vol: ",
      base::round(channel_vol, 2),
      "floodplain vol: ",
      base::round(floodplain_vol, 2)
    ))
    log_message("update results_map -------------------------------------")
    leafletProxy(mapId = "results_map", data = channel_poly) %>%
      flyTo(
        lng = input$results_map_center$lng,
        lat = input$results_map_center$lat,
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
    log_message("update cross section plots -----------------------------")
    output$xs_plot_floodplain <- renderPlot({
      xs_compare_plot_L2(
        stream = "current stream",
        xs_number = input$pick_xs,
        bankfull_elevation = input$channel_elevation,
        xs_pts_list,
        extent = "floodplain",
        aspect_ratio = NULL
      )
    })
    output$xs_plot_channel <- renderPlot({
      xs_compare_plot_L2(
        stream = "current stream",
        xs_number = input$pick_xs,
        bankfull_elevation = input$channel_elevation,
        xs_pts_list,
        extent = "channel",
        aspect_ratio = NULL
      )
    })
    log_message("update discharge ---------------------------------------")
    output$channel_discharge <- render_gt(
      xs_discharge_table(
        xs_pts = xs_pts,
        xs_number = input$pick_xs,
        bf_estimate = input$channel_elevation,
        mannings_n = as.numeric(input$channel_mannings)
      )
    )
    output$floodplain_volumes <- render_gt(
      floodplain_vol_table(channel_vol, floodplain_vol)
    )
  }) ## End Channel Slider Observer ######################

  observeEvent(input$floodplain_elevation, {
    req(results_loaded())
    log_message(paste(
      "Floodplain elevation value:",
      input$floodplain_elevation
    ))
    log_message("update floodplain_elevation ----------------------------")
    floodplain_poly <<- water_surface_poly(
      rem = rem,
      water_surface_elevation = as.numeric(input$floodplain_elevation),
      flowline = fl
    )
    log_message(floodplain_poly)
    xs_pts <<- xs_pts_classify(
      xs_pts,
      channel_poly,
      floodplain_poly,
      buffer_distance = 2
    )
    xs_pts_list <- list("latest" = xs_pts)
    log_message("create floodplain water surface ------------------------")
    log_message(input$floodplain_elevation)
    floodplain_ws <<- trend + (as.numeric(input$floodplain_elevation) - 100)
    log_message(floodplain_ws)
    log_message("calculate floodplain volumes ---------------------------")
    floodplain_vol <<- floodplain_volume(
      dem = dem,
      watersurface = floodplain_ws
    )
    log_message(paste(
      "channel vol: ",
      base::round(channel_vol, 2),
      "floodplain vol: ",
      base::round(floodplain_vol, 2)
    ))
    log_message("update results_map -------------------------------------")
    leafletProxy(mapId = "results_map", data = floodplain_poly) %>%
      flyTo(
        lng = input$results_map_center$lng,
        lat = input$results_map_center$lat,
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
    log_message("update cross section plots -----------------------------")
    output$xs_plot_floodplain <- renderPlot({
      xs_compare_plot_L2(
        stream = "current stream",
        xs_number = input$pick_xs,
        bankfull_elevation = input$channel_elevation,
        xs_pts_list,
        extent = "floodplain",
        aspect_ratio = NULL
      )
    })
    output$xs_plot_channel <- renderPlot({
      xs_compare_plot_L2(
        stream = "current stream",
        xs_number = input$pick_xs,
        bankfull_elevation = input$channel_elevation,
        xs_pts_list,
        extent = "channel",
        aspect_ratio = NULL
      )
    })
    log_message("update discharge ---------------------------------------")
    output$floodplain_discharge <- render_gt(
      xs_discharge_table(
        xs_pts = xs_pts,
        xs_number = input$pick_xs,
        bf_estimate = input$floodplain_elevation,
        mannings_n = as.numeric(input$floodplain_mannings)
      )
    )
    output$floodplain_volumes <- render_gt(
      floodplain_vol_table(channel_vol, floodplain_vol)
    )
  }) ## End Floodplain Slider Observer ###################

  observeEvent(input$channel_mannings, {
    req(results_loaded())
    log_message("update discharge ---------------------------------------")
    output$channel_discharge <- render_gt(
      xs_discharge_table(
        xs_pts = xs_pts,
        xs_number = input$pick_xs,
        bf_estimate = input$channel_elevation,
        mannings_n = as.numeric(input$channel_mannings)
      )
    )
  }) ## End Channel Manning's n update ###################

  observeEvent(input$floodplain_mannings, {
    req(results_loaded())
    log_message("update discharge ---------------------------------------")
    output$floodplain_discharge <- render_gt(
      xs_discharge_table(
        xs_pts = xs_pts,
        xs_number = input$pick_xs,
        bf_estimate = input$floodplain_elevation,
        mannings_n = as.numeric(input$floodplain_mannings)
      )
    )
  }) ## End Floodplain Manning's n update ################

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
