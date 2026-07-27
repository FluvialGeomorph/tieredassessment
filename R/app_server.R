#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @param skin Normalized application skin configuration.
#' @param reach_slope_resolver Injectable USGS/fallback slope resolver.
#' @param dem_slope_resolver Injectable bulk Local XS slope resolver.
#' @param sampled_dem_slope_resolver Injectable Sampled DEM Reach resolver.
#' @param dem_resolver Injectable terrain-service resolver.
#' @param dem_max_span_m Maximum buffered DEM request span in metres.
#' @param polygon_cache_max_entries Maximum cached interactive-flooding levels.
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
  dem_slope_resolver = resolve_local_xs_slope_results,
  sampled_dem_slope_resolver = resolve_sampled_dem_reach_slope,
  dem_resolver = get_dem,
  dem_max_span_m = getOption("ohwm2.max_dem_span_m", 10000),
  polygon_cache_max_entries = getOption(
    "ohwm2.interactive_polygon_cache_size",
    32L
  )
) {
  # Define reactives ##########################################################
  results_loaded <- reactiveVal(FALSE)
  reach_slope_cache <- reactiveVal(NULL)
  sampled_dem_slope_cache <- reactiveVal(NULL)
  dem_slope_cache <- reactiveVal(list())
  channel_polygon_level <- reactiveVal(NULL)
  floodplain_polygon_level <- reactiveVal(NULL)
  channel_analytics_pending <- reactiveVal(FALSE)
  floodplain_analytics_pending <- reactiveVal(FALSE)
  volume_lookup_cache <- reactiveVal(NULL)
  water_surface_polygon_cache <- reactiveVal(
    new_water_surface_polygon_cache(polygon_cache_max_entries)
  )
  channel_xs_pts <- reactiveVal(empty_sf())
  floodplain_xs_pts <- reactiveVal(empty_sf())
  xs_geometry_snapshot <- reactiveVal(NULL)
  flowline_geometry_snapshot <- reactiveVal(NULL)
  flowline_editor_state <- reactiveVal(NULL)
  flowline_editor_generation <- reactiveVal(0L)
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
    if (identical(slope_scale, "dem_xs_local")) {
      dem_slope_cache()[[key]]
    } else if (identical(slope_scale, "dem_reach")) {
      sampled_dem_slope_cache()
    } else {
      reach_slope_cache()
    }
  })

  refresh_dem_slope <- function() {
    xs_pts_value <- isolate(xs_pts)
    results <- tryCatch(
      dem_slope_resolver(xs_pts_value),
      error = function(e) {
        log_message(paste(
          "ERROR resolving Local XS Neighborhood slopes:",
          conditionMessage(e)
        ))
        list()
      }
    )
    dem_slope_cache(results)
    invisible(results)
  }

  refresh_sampled_dem_slope <- function() {
    flowline_pts_value <- isolate(fl_pts)
    result <- tryCatch(
      sampled_dem_slope_resolver(flowline_pts_value),
      error = function(e) {
        log_message(paste(
          "ERROR resolving Sampled DEM Reach slope:",
          conditionMessage(e)
        ))
        new_reach_slope_result(
          value = NA_real_,
          source = "dem_reach",
          status = "unavailable",
          reason = "lookup_error",
          attempts = 0L,
          message = paste(
            "The Sampled DEM Reach slope could not be resolved.",
            "Map, cross-section, and storage results remain available."
          )
        )
      }
    )
    sampled_dem_slope_cache(result)
    invisible(result)
  }

  refresh_reach_slope <- function(
    notify_user = TRUE,
    xs_number = isolate(input$pick_xs)
  ) {
    xs_pts_value <- isolate(xs_pts)
    result <- tryCatch(
      reach_slope_resolver(
        xs_pts_value,
        xs_number,
        fallback_result = isolate(sampled_dem_slope_cache())
      ),
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
    reach_slope_cache(result)

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
        if (identical(input$slope_scale, "dem_xs_local")) {
          "Preparing the Local XS Neighborhood slope."
        } else if (identical(input$slope_scale, "dem_reach")) {
          "Preparing the Sampled DEM Reach slope."
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

  channel_elevation_live <- reactive({
    req(results_loaded())
    as.numeric(input$channel_elevation)
  })
  floodplain_elevation_live <- reactive({
    req(results_loaded())
    as.numeric(input$floodplain_elevation)
  })
  channel_elevation_map <- throttle(channel_elevation_live, millis = 120)
  floodplain_elevation_map <- throttle(
    floodplain_elevation_live,
    millis = 120
  )
  channel_elevation_analytics <- debounce(
    channel_elevation_live,
    millis = 400
  )
  floodplain_elevation_analytics <- debounce(
    floodplain_elevation_live,
    millis = 400
  )

  interactive_flooding_ready <- function() {
    rem_value <- isolate(rem)
    flowline_value <- isolate(fl)
    xs_pts_value <- isolate(xs_pts)

    isTRUE(isolate(results_loaded())) &&
      inherits(rem_value, "SpatRaster") &&
      terra::ncell(rem_value) > 0L &&
      inherits(flowline_value, "sf") &&
      nrow(flowline_value) > 0L &&
      inherits(xs_pts_value, "sf") &&
      nrow(xs_pts_value) > 0L
  }

  update_results_polygon_layer <- function(
    polygon,
    layer_id,
    color,
    group
  ) {
    leafletProxy(mapId = "results_map", session = session) %>%
      removeShape(layerId = layer_id) %>%
      addPolygons(
        data = st_transform(polygon, crs = 4326),
        layerId = layer_id,
        color = color,
        weight = 1,
        group = group
      )
  }

  resolve_interactive_polygon <- function(level) {
    resolved <- resolve_cached_water_surface_polygon(
      cache = isolate(water_surface_polygon_cache()),
      rem_elevation = level,
      rem = isolate(rem),
      flowline = isolate(fl)
    )
    water_surface_polygon_cache(resolved$cache)
    resolved$polygon
  }

  output$results_map <- renderLeaflet({
    req(results_loaded())
    get_results_leaflet(
      isolate(fl),
      isolate(xs),
      isolate(dem),
      isolate(channel_poly),
      isolate(floodplain_poly)
    )
  })

  output$long_profile <- renderPlot({
    req(results_loaded())
    fl_pts_list <- list("latest" = isolate(fl_pts))
    compare_long_profile(stream = "current stream", fl_pts_list)
  })

  output$xs_plot_floodplain <- renderPlot({
    req(results_loaded())
    level <- channel_elevation_analytics()
    req(length(level) == 1L, is.finite(level))
    classified_points <- floodplain_xs_pts()
    req(inherits(classified_points, "sf"), nrow(classified_points) > 0L)
    xs_pts_list <- list("latest" = classified_points)

    xs_compare_plot_L2(
      stream = "current stream",
      xs_number = input$pick_xs,
      bankfull_elevation = level,
      xs_pts_list,
      extent = "floodplain",
      aspect_ratio = NULL
    )
  })

  output$xs_plot_channel <- renderPlot({
    req(results_loaded())
    level <- channel_elevation_analytics()
    req(length(level) == 1L, is.finite(level))
    classified_points <- channel_xs_pts()
    req(inherits(classified_points, "sf"), nrow(classified_points) > 0L)
    xs_pts_list <- list("latest" = classified_points)

    xs_compare_plot_L2(
      stream = "current stream",
      xs_number = input$pick_xs,
      bankfull_elevation = level,
      xs_pts_list,
      extent = "channel",
      aspect_ratio = NULL
    )
  })

  output$channel_discharge <- render_gt({
    req(results_loaded())
    level <- channel_elevation_analytics()
    req(length(level) == 1L, is.finite(level))
    mannings_n <- as.numeric(input$channel_mannings)
    req(length(mannings_n) == 1L, is.finite(mannings_n))
    classified_points <- channel_xs_pts()
    req(inherits(classified_points, "sf"), nrow(classified_points) > 0L)

    render_cached_discharge(
      xs_pts = classified_points,
      xs_number = input$pick_xs,
      bf_estimate = level,
      mannings_n = mannings_n
    )
  })

  output$floodplain_discharge <- render_gt({
    req(results_loaded())
    level <- floodplain_elevation_analytics()
    req(length(level) == 1L, is.finite(level))
    mannings_n <- as.numeric(input$floodplain_mannings)
    req(length(mannings_n) == 1L, is.finite(mannings_n))
    classified_points <- channel_xs_pts()
    req(inherits(classified_points, "sf"), nrow(classified_points) > 0L)

    render_cached_discharge(
      xs_pts = classified_points,
      xs_number = input$pick_xs,
      bf_estimate = level,
      mannings_n = mannings_n
    )
  })

  output$floodplain_volumes <- render_gt({
    req(results_loaded())
    req(!is.null(channel_vol()), !is.null(floodplain_vol()))
    floodplain_vol_table(channel_vol(), floodplain_vol())
  })

  invalidate_downstream_results <- function() {
    results_loaded(FALSE)
    reach_slope_cache(NULL)
    sampled_dem_slope_cache(NULL)
    dem_slope_cache(list())
    channel_polygon_level(NULL)
    floodplain_polygon_level(NULL)
    channel_analytics_pending(FALSE)
    floodplain_analytics_pending(FALSE)
    volume_lookup_cache(NULL)
    water_surface_polygon_cache(
      new_water_surface_polygon_cache(polygon_cache_max_entries)
    )
    channel_xs_pts(empty_sf())
    floodplain_xs_pts(empty_sf())
    invisible(NULL)
  }

  # Draw XS ###################################################################
  # Define the leaflet draw_xs_map
  draw_xs_map <- get_draw_xs_leaflet(zoom = 4)

  # Define the draw_xs mapedit module
  xs_editor_ui <- callModule(
    editMod,
    id = "xs_editor_ui_id",
    leafmap = draw_xs_map,
    targetLayerId = NULL,
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

  output$draw_fl_button <- renderUI({
    editor_geometry <- xs_editor_ui()$finished
    req(inherits(editor_geometry, "sf"), nrow(editor_geometry) > 0L)

    actionButton(
      "draw_flowline",
      skin$workflow$draw_xs$next_button
    )
  })

  observeEvent(xs_editor_ui()$finished, {
    log_message("Cross section drawn.")
  })

  output$view_results_button <- renderUI({
    editor <- flowline_editor_state()
    req(is.function(editor))
    editor_geometry <- editor()$finished
    req(inherits(editor_geometry, "sf"), nrow(editor_geometry) > 0L)

    actionButton(
      "view_results",
      skin$workflow$draw_flowline$next_button
    )
  })

  # Draw Flowline #############################################################
  observeEvent(input$draw_flowline, {
    invalidate_downstream_results()
    show_modal_spinner(
      spin = "circle",
      text = skin$workflow$draw_xs$progress_message
    )
    on.exit(try(remove_modal_spinner(), silent = TRUE), add = TRUE)

    tryCatch(
      {
        xs_mapedit <- xs_editor_ui()$finished
        log_message(
          "mapedit xs -----------------------------------------------"
        )
        log_message(xs_mapedit)
        xs_mapedit <- sf_fix_crs(xs_mapedit)
        xs_snapshot <- prepare_cross_section_geometry_snapshot(xs_mapedit)

        preflight <- prepare_dem_request_preflight(
          xs = xs_snapshot,
          max_span_m = dem_max_span_m
        )
        if (!preflight$ok) {
          showNotification(
            preflight$message,
            type = "error",
            duration = 15
          )
          log_message(paste(
            "DEM preflight rejected:",
            preflight$reason,
            preflight$request_span_m
          ))
          return(invisible(NULL))
        }

        dem_value <- tryCatch(
          dem_resolver(xs_snapshot),
          error = function(e) {
            classified <- classify_dem_request_error(e)
            log_message(paste(
              "ERROR retrieving DEM:",
              conditionMessage(e)
            ))
            showNotification(
              classified$message,
              type = "error",
              duration = 15
            )
            NULL
          }
        )
        if (is.null(dem_value)) {
          return(invisible(NULL))
        }
        if (!dem_has_finite_elevations(dem_value)) {
          showNotification(
            paste(
              "The terrain service returned no usable elevations for this",
              "site. Move the cross sections to a covered area and try again."
            ),
            type = "error",
            duration = 15
          )
          log_message("DEM response contained no finite elevations.")
          return(invisible(NULL))
        }

        # Commit one immutable geometry/terrain snapshot only after the
        # current editor state and DEM response both validate.
        xs_geometry_snapshot(xs_snapshot)
        flowline_geometry_snapshot(NULL)
        xs <<- xs_snapshot
        dem <<- dem_value
        log_message(
          "Committed cross-section snapshot -------------------------"
        )
        log_message(xs_snapshot)
        log_message(
          "Returned DEM ---------------------------------------------"
        )
        log_message(dem_value)

        terrain_map <- get_terrain_leaflet(xs_snapshot, dem_value)
        generation <- isolate(flowline_editor_generation()) + 1L
        flowline_editor_generation(generation)
        editor_id <- paste0("fl_editor_ui_id_", generation)

        output$flowline_editor_ui <- renderUI({
          editModUI(id = editor_id)
        })
        editor <- callModule(
          editMod,
          id = editor_id,
          leafmap = terrain_map,
          targetLayerId = NULL,
          crs = 4326,
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
            drawOptions = pmDrawOptions(
              snappable = FALSE,
              tooltips = FALSE
            )
          )
        )
        flowline_editor_state(editor)

        nav_select(id = "main", selected = "draw_flowline", session)
      },
      error = function(e) {
        log_message(paste(
          "ERROR preparing Draw Flowline:",
          conditionMessage(e)
        ))
        showNotification(
          paste(
            "Draw Flowline could not be prepared from the current cross",
            "sections. Review the geometry and try again."
          ),
          type = "error",
          duration = 15
        )
      }
    )
  })

  observeEvent(input$view_results, {
    invalidate_downstream_results()
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
        editor <- isolate(flowline_editor_state())
        if (!is.function(editor)) {
          stop(
            "Return to Draw Flowline and draw a flowline before continuing.",
            call. = FALSE
          )
        }
        fl_mapedit <- editor()$finished
        log_message(fl_mapedit)
        fl_mapedit <- sf_fix_crs(fl_mapedit)
        fl_snapshot <- prepare_flowline_geometry_snapshot(fl_mapedit)

        log_message(
          "Digitized flowline ---------------------------------------"
        )
        log_message(fl_snapshot)

        log_message(
          "process flowline -----------------------------------------"
        )
        xs_snapshot <- isolate(xs_geometry_snapshot())
        if (!inherits(xs_snapshot, "sf") || nrow(xs_snapshot) == 0L) {
          stop(
            paste(
              "Return to Draw XS and submit the cross sections before",
              "continuing."
            ),
            call. = FALSE
          )
        }
        dem_snapshot <- isolate(dem)
        flowline_geometry_snapshot(fl_snapshot)
        log_message(dem_snapshot)
        fl <<- flowline(
          fl_snapshot,
          reach_name = "current stream",
          dem_snapshot
        )
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
        volume_lookup_cache(prepare_floodplain_volume_lookup(dem, trend))
        log_message(rem)

        log_message(
          "create channel and floodplain polys ----------------------"
        )
        channel_level <- as.numeric(isolate(input$channel_elevation))
        floodplain_level <- as.numeric(isolate(input$floodplain_elevation))
        channel_poly <<- resolve_interactive_polygon(channel_level)
        channel_polygon_level(channel_level)
        floodplain_poly <<- resolve_interactive_polygon(floodplain_level)
        floodplain_polygon_level(floodplain_level)
        log_message(channel_poly)
        log_message(floodplain_poly)

        log_message(
          "process cross section ------------------------------------"
        )
        xs <<- cross_section(xs_snapshot, fl_pts, watershed = "skip")
        log_message(xs)

        log_message(
          "process cross section points -----------------------------"
        )
        station_distance <- 1
        xs_pts <<- cross_section_points(xs, dem, rem, station_distance)
        xs_pts <<- xs_pts %>%
          mutate(POINT_M_units = "m") %>%
          mutate(dem_units = "ft")
        channel_xs_pts(update_xs_polygon_classification(
          xs_pts = xs_pts,
          polygon = channel_poly,
          field = "channel",
          buffer_distance = 2
        ))
        floodplain_xs_pts(update_xs_polygon_classification(
          xs_pts = xs_pts,
          polygon = floodplain_poly,
          field = "floodplain",
          buffer_distance = 2
        ))
        log_message(xs_pts)

        log_message(
          "cache DEM slope scales -----------------------------------"
        )
        refresh_sampled_dem_slope()
        refresh_dem_slope()

        log_message(
          "calculate floodplain volumes -----------------------------"
        )
        channel_vol(calculate_floodplain_volume(
          volume_lookup_cache(),
          as.numeric(isolate(input$channel_elevation))
        ))
        floodplain_vol(calculate_floodplain_volume(
          volume_lookup_cache(),
          as.numeric(isolate(input$floodplain_elevation))
        ))

        log_message(paste(
          "channel vol: ",
          base::round(channel_vol(), 2),
          "floodplain vol: ",
          base::round(floodplain_vol(), 2)
        ))

        log_message(
          "create results map ---------------------------------------"
        )

        log_message(
          "longitudinal profile plot --------------------------------"
        )

        log_message(
          "create cross section plots -------------------------------"
        )

        log_message(
          "calculate volumes ----------------------------------------"
        )

        log_message(
          "calculate discharge --------------------------------------"
        )

        log_message("pick cross section -------------------------------------")
        log_message(input$pick_xs)

        xs_pts_value <- isolate(xs_pts)

        transition_state <- run_results_workflow_transition(
          session = session,
          input = input,
          xs_pts = xs_pts_value,
          set_results_loaded = results_loaded
        )

        if (length(transition_state$unavailable_cross_sections) > 0L) {
          showNotification(
            paste0(
              "Cross section",
              if (length(transition_state$unavailable_cross_sections) > 1L) {
                "s "
              } else {
                " "
              },
              paste(
                transition_state$unavailable_cross_sections,
                collapse = ", "
              ),
              if (length(transition_state$unavailable_cross_sections) > 1L) {
                " were"
              } else {
                " was"
              },
              paste(
                " omitted because the sampled terrain did not provide a",
                "usable Results elevation range."
              )
            ),
            type = "warning",
            duration = 12
          )
        }

        log_message(paste0(
          "transition complete; pick_xs = ",
          transition_state$pick_xs
        ))

        nav_select(id = "main", selected = "results", session)
        remove_modal_spinner()
        session$onFlushed(
          function() {
            if (!read_deferred_results_gate(results_loaded)) {
              return(invisible(NULL))
            }

            show_modal_spinner(
              spin = "circle",
              text = skin$workflow$results$progress_message
            )

            # Defer the resolver work for one more flush so the progress
            # message reaches the browser before a remote lookup blocks.
            session$onFlushed(
              function() {
                on.exit(
                  try(remove_modal_spinner(), silent = TRUE),
                  add = TRUE
                )
                if (!read_deferred_results_gate(results_loaded)) {
                  return(invisible(NULL))
                }

                selected_xs <- transition_state$pick_xs
                if (
                  identical(isolate(input$slope_scale), "usgs_reach") &&
                    is.null(isolate(reach_slope_cache()))
                ) {
                  refresh_reach_slope(xs_number = selected_xs)
                }
              },
              once = TRUE
            )
            session$requestFlush()
          },
          once = TRUE
        )
      },
      error = function(e) {
        log_message(paste("ERROR in view_results:", conditionMessage(e)))
        log_message(paste("ERROR call:", deparse(conditionCall(e))))
        log_message(paste(capture.output(str(e)), collapse = "\n"))
        showNotification(
          paste(
            "Results could not be calculated from the current cross sections",
            "and flowline. Review the geometry and try again."
          ),
          type = "error",
          duration = 15
        )
      }
    )
  }) ## End View Results #################################

  observeEvent(input$channel_elevation, {
    if (interactive_flooding_ready()) {
      channel_analytics_pending(TRUE)
    }
  }, ignoreInit = TRUE, priority = 200)

  observeEvent(input$floodplain_elevation, {
    if (interactive_flooding_ready()) {
      floodplain_analytics_pending(TRUE)
    }
  }, ignoreInit = TRUE, priority = 200)

  observeEvent(channel_elevation_map(), {
    if (!interactive_flooding_ready()) {
      return(invisible(NULL))
    }
    level <- channel_elevation_map()
    req(length(level) == 1L, is.finite(level))

    log_message(paste("Interactive channel REM:", level))
    polygon <- resolve_interactive_polygon(level)
    channel_poly <<- polygon
    channel_polygon_level(level)
    update_results_polygon_layer(
      polygon = polygon,
      layer_id = "channel_poly",
      color = "navy",
      group = "Channel"
    )
  }, ignoreInit = TRUE, priority = 100)

  observeEvent(floodplain_elevation_map(), {
    if (!interactive_flooding_ready()) {
      return(invisible(NULL))
    }
    level <- floodplain_elevation_map()
    req(length(level) == 1L, is.finite(level))

    log_message(paste("Interactive floodplain REM:", level))
    polygon <- resolve_interactive_polygon(level)
    floodplain_poly <<- polygon
    floodplain_polygon_level(level)
    update_results_polygon_layer(
      polygon = polygon,
      layer_id = "floodplain_poly",
      color = "forestgreen",
      group = "Floodplain"
    )
  }, ignoreInit = TRUE, priority = 100)

  observeEvent(channel_elevation_analytics(), {
    if (!interactive_flooding_ready()) {
      return(invisible(NULL))
    }
    on.exit(channel_analytics_pending(FALSE), add = TRUE)
    level <- channel_elevation_analytics()
    req(length(level) == 1L, is.finite(level))
    req(!is.null(input$channel_mannings))

    if (!identical(isolate(channel_polygon_level()), level)) {
      polygon <- resolve_interactive_polygon(level)
      channel_poly <<- polygon
      channel_polygon_level(level)
      update_results_polygon_layer(
        polygon = polygon,
        layer_id = "channel_poly",
        color = "navy",
        group = "Channel"
      )
    }

    log_message(paste("Settle channel REM analytics:", level))
    channel_xs_pts(update_xs_polygon_classification(
      xs_pts = isolate(xs_pts),
      polygon = isolate(channel_poly),
      field = "channel",
      buffer_distance = 2
    ))
    channel_vol(calculate_floodplain_volume(
      isolate(volume_lookup_cache()),
      level
    ))
  }, ignoreInit = TRUE)

  observeEvent(floodplain_elevation_analytics(), {
    if (!interactive_flooding_ready()) {
      return(invisible(NULL))
    }
    on.exit(floodplain_analytics_pending(FALSE), add = TRUE)
    level <- floodplain_elevation_analytics()
    req(length(level) == 1L, is.finite(level))
    req(!is.null(input$floodplain_mannings))

    if (!identical(isolate(floodplain_polygon_level()), level)) {
      polygon <- resolve_interactive_polygon(level)
      floodplain_poly <<- polygon
      floodplain_polygon_level(level)
      update_results_polygon_layer(
        polygon = polygon,
        layer_id = "floodplain_poly",
        color = "forestgreen",
        group = "Floodplain"
      )
    }

    log_message(paste("Settle floodplain REM analytics:", level))
    floodplain_xs_pts(update_xs_polygon_classification(
      xs_pts = isolate(xs_pts),
      polygon = isolate(floodplain_poly),
      field = "floodplain",
      buffer_distance = 2
    ))
    floodplain_vol(calculate_floodplain_volume(
      isolate(volume_lookup_cache()),
      level
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$pick_xs, {
    req(results_loaded())
    slider_state <- prepare_results_slider_state(
      xs_pts = isolate(xs_pts),
      pick_xs = input$pick_xs,
      channel_elevation = isolate(input$channel_elevation),
      floodplain_elevation = isolate(input$floodplain_elevation)
    )
    updateSliderInput(
      session,
      "channel_elevation",
      value = slider_state$channel_elevation_value,
      min = slider_state$rem_min,
      max = slider_state$rem_max
    )
    updateSliderInput(
      session,
      "floodplain_elevation",
      value = slider_state$floodplain_elevation_value,
      min = slider_state$rem_min,
      max = slider_state$rem_max
    )
  }, ignoreInit = TRUE, priority = 150)

  observeEvent(input$slope_scale, {
    req(results_loaded())
    if (
      identical(input$slope_scale, "usgs_reach") &&
        is.null(isolate(reach_slope_cache()))
    ) {
      refresh_reach_slope()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$retry_usgs_slope, {
    req(results_loaded())
    refresh_reach_slope()
  }, ignoreInit = TRUE)

  output$interactive_flooding_status <- renderUI({
    req(results_loaded())
    if (
      !isTRUE(channel_analytics_pending()) &&
        !isTRUE(floodplain_analytics_pending())
    ) {
      return(NULL)
    }

    tags$div(
      class = "text-muted small mb-2",
      role = "status",
      tags$strong("Updating interactive flooding."),
      " Plots, storage, and discharge follow when the slider settles\u2026"
    )
  })

  output$discharge_service_status <- renderUI({
    req(results_loaded())
    slope_result <- current_reach_slope()
    slope_scale <- input$slope_scale
    if (is.null(slope_scale)) {
      slope_scale <- "usgs_reach"
    }

    if (is.null(slope_result)) {
      pending_label <- switch(
        slope_scale,
        dem_xs_local = "Preparing Local XS Neighborhood slope",
        dem_reach = "Preparing Sampled DEM Reach slope",
        "Checking USGS stream-network data"
      )
      return(tags$div(
        class = "alert alert-info py-2",
        tags$strong(pending_label),
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
          ". This reach-wide value applies to every cross section."
        ))
      ))
    }

    if (
      slope_result$status == "available" &&
        slope_result$source == "dem_reach"
    ) {
      return(tags$div(
        class = "alert alert-info py-2",
        tags$strong("Sampled DEM Reach slope applied"),
        tags$div(paste0(
          "Applied S = ", slope_value,
          ". This reach-wide value applies to every cross section."
        ))
      ))
    }

    if (
      slope_result$status == "available" &&
        slope_result$source == "dem_xs_local"
    ) {
      return(tags$div(
        class = "alert alert-info py-2",
        tags$strong("Local XS Neighborhood slope applied"),
        tags$div(paste0(
          "Applied S = ", slope_value,
          ". This value is centered on the selected cross section."
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
        "USGS unavailable - Sampled DEM Reach slope applied"
      } else {
        "Discharge temporarily unavailable"
      }),
      if (slope_result$status == "fallback") {
        tags$div(paste0("Applied S = ", slope_value, "."))
      },
      tags$div(slope_result$message),
      if (identical(slope_scale, "usgs_reach")) {
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
