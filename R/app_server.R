#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @param skin Normalized application skin configuration.
#' @param reach_slope_resolver Injectable USGS/fallback slope resolver.
#' @param dem_slope_resolver Injectable local DEM slope resolver.
#' @import shiny
#' @importFrom bslib nav_select
#' @importFrom htmltools tags
#' @importFrom purrr map
#' @import leaflet
#' @importFrom dplyr %>% bind_rows mutate select filter distinct
#' @importFrom mapedit editMod
#' @importFrom leafpm addPmToolbar pmToolbarOptions pmDrawOptions
#' @importFrom leaflet.extras addSearchOSM searchOptions
#' @importFrom sf st_as_sf st_sfc st_transform
#' @importFrom terra plot crs ifel as.polygons disagg relate vect
#' @importFrom tidyterra filter mutate
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @import fluvgeo
#' @importFrom gt render_gt
#' @noRd
app_server <- function(
  input,
  output,
  session,
  skin = load_app_skin(),
  reach_slope_resolver = resolve_reach_slope,
  dem_slope_resolver = resolve_dem_reach_slope
) {
  # Define reactives ##########################################################
  results_loaded <- reactiveVal(FALSE)
  reach_slope_cache <- reactiveVal(list())
  dem_slope_cache <- reactiveVal(list())
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

  current_reach_slope <- reactive({
    key <- as.character(input$pick_xs)
    slope_scale <- input$slope_scale
    if (is.null(slope_scale)) {
      slope_scale <- "usgs_reach"
    }
    if (identical(slope_scale, "dem_local")) {
      dem_slope_cache()[[key]]
    } else {
      reach_slope_cache()[[key]]
    }
  })

  refresh_dem_slope <- function() {
    xs_number <- isolate(input$pick_xs)
    xs_pts_value <- isolate(xs_pts)
    result <- tryCatch(
      dem_slope_resolver(xs_pts_value, xs_number),
      error = function(e) {
        log_message(paste(
          "ERROR resolving local DEM slope:",
          conditionMessage(e)
        ))
        new_reach_slope_result(
          value = NA_real_,
          source = "dem_local",
          status = "unavailable",
          reason = "lookup_error",
          attempts = 0L,
          message = paste(
            "The local DEM slope could not be resolved.",
            "Map, cross-section, and storage results remain available."
          )
        )
      }
    )
    cache <- isolate(dem_slope_cache())
    cache[[as.character(xs_number)]] <- result
    dem_slope_cache(cache)
    invisible(result)
  }

  refresh_reach_slope <- function(notify_user = TRUE) {
    xs_number <- isolate(input$pick_xs)
    xs_pts_value <- isolate(xs_pts)
    result <- tryCatch(
      reach_slope_resolver(xs_pts_value, xs_number),
      error = function(e) {
        log_message(paste(
          "ERROR resolving reach slope:",
          conditionMessage(e)
        ))
        new_reach_slope_result(
          value = NA_real_,
          source = NA_character_,
          status = "unavailable",
          reason = "lookup_error",
          attempts = 0L,
          message = paste(
            "Discharge is temporarily unavailable because a reach slope",
            "could not be resolved. Map, cross-section, and storage results",
            "remain available."
          )
        )
      }
    )
    cache <- isolate(reach_slope_cache())
    cache[[as.character(xs_number)]] <- result
    reach_slope_cache(cache)

    if (notify_user && result$status != "available") {
      showNotification(
        result$message,
        type = if (result$status == "fallback") "warning" else "error",
        duration = 12
      )
    }

    invisible(result)
  }

  render_cached_discharge <- function(
    xs_pts,
    xs_number,
    bf_estimate,
    mannings_n
  ) {
    slope_result <- current_reach_slope()
    if (is.null(slope_result)) {
      return(discharge_unavailable_table(paste(
        if (identical(input$slope_scale, "dem_local")) {
          "Preparing the selected cross section's local DEM slope."
        } else {
          "Checking USGS stream-network coverage."
        },
        "Map, cross-section, and storage results are ready."
      )))
    }

    xs_discharge_table(
      xs_pts = xs_pts,
      xs_number = xs_number,
      bf_estimate = bf_estimate,
      mannings_n = mannings_n,
      reach_slope_result = slope_result
    )
  }

  # Ensure fl_editor_ui mapedit module available at app scope
  fl_editor_ui <- NULL
  makeReactiveBinding("fl_editor_ui")

  # Draw XS ###################################################################
  # Define the leaflet draw_xs_map
  draw_xs_map <- get_draw_xs_leaflet(zoom = 4)

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
      actionButton(
        "draw_flowline",
        skin$workflow$draw_xs$next_button
      )
    })
    log_message("Cross section drawn.")
  })

  # Draw Flowline #############################################################
  observeEvent(input$draw_flowline, {
    show_modal_spinner(
      spin = "circle",
      text = skin$workflow$draw_xs$progress_message
    )
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
    nav_select(id = "main", selected = "draw_flowline", session)
    remove_modal_spinner()

    observeEvent(fl_editor_ui()$finished, {
      output$view_results_button <- renderUI({
        actionButton(
          "view_results",
          skin$workflow$draw_flowline$next_button
        )
      })
    })
  })

  observeEvent(input$view_results, {
    reach_slope_cache(list())
    dem_slope_cache(list())
    show_modal_spinner(
      spin = "circle",
      text = skin$workflow$draw_flowline$progress_message
    )
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
        xs <<- cross_section(xs, fl_pts, watershed = "skip")
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

          render_cached_discharge(
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

          render_cached_discharge(
            xs_pts = xs_pts,
            xs_number = input$pick_xs,
            bf_estimate = input$floodplain_elevation,
            mannings_n = as.numeric(input$floodplain_mannings)
          )
        })

        log_message("pick cross section -------------------------------------")
        log_message(input$pick_xs)

        xs_pts_value <- isolate(xs_pts)

        transition_state <- run_results_workflow_transition(
          session = session,
          input = input,
          xs_pts = xs_pts_value,
          set_results_loaded = results_loaded
        )

        log_message(paste0(
          "transition complete; pick_xs = ",
          transition_state$pick_xs
        ))

        nav_select(id = "main", selected = "results", session)
        remove_modal_spinner()
        session$onFlushed(
          function() {
            if (read_deferred_results_gate(results_loaded)) {
              key <- as.character(isolate(input$pick_xs))
              if (is.null(isolate(dem_slope_cache())[[key]])) {
                refresh_dem_slope()
              }
              if (
                !identical(isolate(input$slope_scale), "dem_local") &&
                  is.null(isolate(reach_slope_cache())[[key]])
              ) {
                refresh_reach_slope()
              }
            }
          },
          once = TRUE
        )
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
    req(!is.null(input$channel_mannings))
    log_message(paste("Channel elevation value:", input$channel_elevation))
    
    # Validate state capture via helper
    update_state <- prepare_channel_elevation_update(
      channel_elevation = input$channel_elevation,
      pick_xs = input$pick_xs,
      xs_pts = isolate(xs_pts),
      mannings_n = input$channel_mannings
    )
    
    log_message("update channel_elevation -------------------------------")
    channel_poly <<- water_surface_poly(
      rem = rem,
      water_surface_elevation = as.numeric(update_state$channel_elevation_value),
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
    log_message(update_state$channel_elevation_value)
    channel_ws <<- trend + (as.numeric(update_state$channel_elevation_value) - 100)
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
        bankfull_elevation = update_state$channel_elevation_value,
        xs_pts_list,
        extent = "floodplain",
        aspect_ratio = NULL
      )
    })
    output$xs_plot_channel <- renderPlot({
      xs_compare_plot_L2(
        stream = "current stream",
        xs_number = input$pick_xs,
        bankfull_elevation = update_state$channel_elevation_value,
        xs_pts_list,
        extent = "channel",
        aspect_ratio = NULL
      )
    })
    log_message("update discharge ---------------------------------------")
    output$channel_discharge <- render_gt(
      render_cached_discharge(
        xs_pts = xs_pts,
        xs_number = input$pick_xs,
        bf_estimate = update_state$channel_elevation_value,
        mannings_n = as.numeric(update_state$mannings_n)
      )
    )
    output$floodplain_volumes <- render_gt(
      floodplain_vol_table(channel_vol, floodplain_vol)
    )
  }) ## End Channel Slider Observer ######################

  observeEvent(input$floodplain_elevation, {
    req(results_loaded())
    req(!is.null(input$floodplain_mannings))
    log_message(paste(
      "Floodplain elevation value:",
      input$floodplain_elevation
    ))
    
    # Validate state capture via helper
    update_state <- prepare_floodplain_elevation_update(
      floodplain_elevation = input$floodplain_elevation,
      pick_xs = input$pick_xs,
      xs_pts = isolate(xs_pts),
      mannings_n = input$floodplain_mannings
    )
    
    log_message("update floodplain_elevation ----------------------------")
    floodplain_poly <<- water_surface_poly(
      rem = rem,
      water_surface_elevation = as.numeric(update_state$floodplain_elevation_value),
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
    log_message(update_state$floodplain_elevation_value)
    floodplain_ws <<- trend + (as.numeric(update_state$floodplain_elevation_value) - 100)
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
      render_cached_discharge(
        xs_pts = xs_pts,
        xs_number = input$pick_xs,
        bf_estimate = update_state$floodplain_elevation_value,
        mannings_n = as.numeric(update_state$mannings_n)
      )
    )
    output$floodplain_volumes <- render_gt(
      floodplain_vol_table(channel_vol, floodplain_vol)
    )
  }) ## End Floodplain Slider Observer ###################

observeEvent(input$channel_mannings, {
    req(results_loaded())
    req(!is.null(input$channel_mannings))
    log_message("update discharge ---------------------------------------")
    
    # Validate state capture via helper
    update_state <- prepare_channel_mannings_update(
      channel_elevation = input$channel_elevation,
      channel_mannings = input$channel_mannings,
      pick_xs = input$pick_xs,
      xs_pts = isolate(xs_pts)
    )
    
    output$channel_discharge <- render_gt(
      render_cached_discharge(
        xs_pts = xs_pts,
        xs_number = input$pick_xs,
        bf_estimate = input$channel_elevation,
        mannings_n = as.numeric(update_state$channel_mannings_value)
      )
    )
  }) ## End Channel Manning's n update ###################

  observeEvent(input$floodplain_mannings, {
    req(results_loaded())
    req(!is.null(input$floodplain_mannings))
    log_message("update discharge ---------------------------------------")
    
    # Validate state capture via helper
    update_state <- prepare_floodplain_mannings_update(
      floodplain_elevation = input$floodplain_elevation,
      floodplain_mannings = input$floodplain_mannings,
      pick_xs = input$pick_xs,
      xs_pts = isolate(xs_pts)
    )
    
    output$floodplain_discharge <- render_gt(
      render_cached_discharge(
        xs_pts = xs_pts,
        xs_number = input$pick_xs,
        bf_estimate = input$floodplain_elevation,
        mannings_n = as.numeric(update_state$floodplain_mannings_value)
      )
    )
  }) ## End Floodplain Manning's n update ################

  observeEvent(input$pick_xs, {
    req(results_loaded())
    key <- as.character(input$pick_xs)
    if (is.null(isolate(dem_slope_cache())[[key]])) {
      refresh_dem_slope()
    }
    if (
      !identical(isolate(input$slope_scale), "dem_local") &&
        is.null(isolate(reach_slope_cache())[[key]])
    ) {
      refresh_reach_slope()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$slope_scale, {
    req(results_loaded())
    key <- as.character(input$pick_xs)
    if (identical(input$slope_scale, "dem_local")) {
      if (is.null(isolate(dem_slope_cache())[[key]])) {
        refresh_dem_slope()
      }
    } else if (is.null(isolate(reach_slope_cache())[[key]])) {
      refresh_reach_slope()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$retry_usgs_slope, {
    req(results_loaded())
    refresh_reach_slope()
  }, ignoreInit = TRUE)

  output$discharge_service_status <- renderUI({
    req(results_loaded())
    slope_result <- current_reach_slope()
    slope_scale <- input$slope_scale
    if (is.null(slope_scale)) {
      slope_scale <- "usgs_reach"
    }

    if (is.null(slope_result)) {
      return(tags$div(
        class = "alert alert-info py-2",
        tags$strong(if (identical(slope_scale, "dem_local")) {
          "Preparing local DEM slope"
        } else {
          "Checking USGS stream-network data"
        }),
        tags$div(
          "Other Results remain ready while the slope is resolved."
        )
      ))
    }

    slope_value <- if (
      length(slope_result$value) == 1L &&
        is.numeric(slope_result$value) &&
        is.finite(slope_result$value)
    ) {
      formatC(slope_result$value, format = "fg", digits = 6)
    } else {
      "not available"
    }

    if (
      slope_result$status == "available" &&
        slope_result$source == "usgs_nhdplus"
    ) {
      return(tags$div(
        class = "alert alert-success py-2",
        tags$strong("USGS Reach slope applied"),
        tags$div(paste0(
          "Applied S = ", slope_value,
          ". The reach-scale result is cached for this cross section."
        ))
      ))
    }

    if (
      slope_result$status == "available" &&
        slope_result$source == "dem_local"
    ) {
      return(tags$div(
        class = "alert alert-info py-2",
        tags$strong("Local DEM slope applied"),
        tags$div(paste0(
          "Applied S = ", slope_value,
          ". This is the signed slope at the selected cross section."
        ))
      ))
    }

    tags$div(
      class = if (slope_result$status == "fallback") {
        "alert alert-warning py-2"
      } else {
        "alert alert-danger py-2"
      },
      tags$strong(if (slope_result$status == "fallback") {
        "USGS unavailable - Local DEM slope applied"
      } else {
        "Discharge temporarily unavailable"
      }),
      if (slope_result$status == "fallback") {
        tags$div(paste0("Applied S = ", slope_value, "."))
      },
      tags$div(slope_result$message),
      if (!identical(slope_scale, "dem_local")) {
        actionButton(
          "retry_usgs_slope",
          "Retry USGS slope",
          class = "btn-sm mt-2"
        )
      }
    )
  })

  # Instructions ##############################################################
  ## create draw xs page instructions
  output$draw_xs_instructions <- renderUI({
    steps <- skin$workflow$draw_xs$instructions
    ul <- htmltools::tags$ul(purrr::map(steps, function(.x) {
      tags$li(.x)
    }))
  })

  ## create draw flowline page instructions
  output$draw_fl_instructions <- renderUI({
    steps <- skin$workflow$draw_flowline$instructions
    ul <- htmltools::tags$ul(purrr::map(steps, function(.x) {
      tags$li(.x)
    }))
  })
}
