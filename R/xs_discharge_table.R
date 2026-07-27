#' @title Create a Cross Section Discharge Table
#'
#' @description
#' Creates a cross section discharge table for the channel portion of the
#' specified cross section.
#'
#' @export
#' @param xs_pts        sf; A cross section lines feature class.
#' @param xs_number     integer; The cross section `Seq` number of the
#'                      requested cross section.
#' @param bf_estimate   numeric; Detrended bankfull estimate (units:
#'                      detrended feet).
#' @param mannings_n    numeric; The Manning's n coeficient.
#' @param reach_slope_result A result returned by `resolve_reach_slope()`.
#'                           When omitted, the reach slope is resolved before
#'                           the table is calculated.
#'
#' @return a `gt` object
#'
#' @importFrom fluvgeo slope_sinuosity xs_geometry
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom nhdplusTools discover_nhdplus_id subset_nhdplus
#' @importFrom gt gt fmt_number cols_label_with cols_label tab_options px
#'
xs_discharge_table <- function(
  xs_pts,
  xs_number,
  bf_estimate,
  mannings_n,
  reach_slope_result = NULL
) {
  if (is.null(reach_slope_result)) {
    reach_slope_result <- resolve_reach_slope(xs_pts, xs_number)
  }

  if (!is_usable_reach_slope(reach_slope_result$value)) {
    return(discharge_unavailable_table(reach_slope_result$message))
  }

  dims_table_long <- prepare_xs_discharge_values(
    xs_pts = xs_pts,
    xs_number = xs_number,
    bf_estimate = bf_estimate,
    mannings_n = mannings_n,
    nhd_slope = reach_slope_result$value
  )
  
  gt_table <- dims_table_long |>
    gt() |>
    cols_label_with(fn = tools::toTitleCase) |>
    cols_label(label = "Variable") |>
    fmt_number(columns = value, decimals = 1) |>
    fmt_number(columns = value, rows = label == "Slope (S)", decimals = 4) |>
    tab_options(
      column_labels.font.weight = "bold",
      table.font.size = "small",
      column_labels.padding = px(2),
      data_row.padding = px(1),
      table.margin.left = px(1),
      table.margin.right = px(1)) |>
    gt::tab_source_note(source_note = paste0(
      "Slope source: ",
      if (reach_slope_result$source == "usgs_nhdplus") {
        "USGS Reach"
      } else if (reach_slope_result$source == "dem_reach") {
        if (reach_slope_result$status == "fallback") {
          "Sampled DEM Reach (automatic fallback)"
        } else {
          "Sampled DEM Reach"
        }
      } else {
        "Local XS Neighborhood"
      },
      " (S = ",
      formatC(reach_slope_result$value, format = "fg", digits = 6),
      ")"
    ))
  #gt_table
  return(gt_table)
}

#' Resolve a reach slope with bounded USGS retries and a DEM fallback
#'
#' @param xs_pts Cross-section points.
#' @param xs_number Cross-section sequence number.
#' @param lookup_fun Function that retrieves a USGS NHDPlus slope for a point.
#' @param fallback_result Precomputed DEM slope result used when USGS fails.
#' @param sleep_fun Function used between retry attempts.
#' @param max_attempts Maximum number of service attempts.
#' @param retry_delays Seconds to wait after failed attempts.
#'
#' @return A structured reach-slope result.
#' @noRd
resolve_reach_slope <- function(
  xs_pts,
  xs_number,
  lookup_fun = lookup_usgs_reach_slope,
  fallback_result = NULL,
  sleep_fun = Sys.sleep,
  max_attempts = 3L,
  retry_delays = c(0.5, 1.5)
) {
  max_attempts <- as.integer(max_attempts)
  if (length(max_attempts) != 1L || is.na(max_attempts) || max_attempts < 1L) {
    stop("max_attempts must be a positive integer")
  }

  service_point <- prepare_usgs_lookup_point(xs_pts, xs_number)
  if (is.null(fallback_result)) {
    fallback_result <- resolve_dem_reach_slope(xs_pts, xs_number)
  }
  last_error <- NULL
  outside_coverage <- FALSE
  attempts_used <- 0L

  for (attempt in seq_len(max_attempts)) {
    attempts_used <- attempt
    service_slope <- tryCatch(
      lookup_fun(service_point),
      usgs_no_coverage = function(e) {
        outside_coverage <<- TRUE
        NA_real_
      },
      error = function(e) {
        last_error <<- e
        NA_real_
      }
    )

    if (is_usable_reach_slope(service_slope)) {
      return(new_reach_slope_result(
        value = service_slope,
        source = "usgs_nhdplus",
        status = "available",
        reason = NULL,
        attempts = attempt,
        message = "USGS NHDPlus reach slope is available."
      ))
    }

    if (outside_coverage) {
      break
    }

    if (attempt < max_attempts) {
      delay_index <- min(attempt, length(retry_delays))
      if (length(retry_delays) > 0L && retry_delays[[delay_index]] > 0) {
        sleep_fun(retry_delays[[delay_index]])
      }
    }
  }

  fallback_slope <- fallback_result$value
  reason <- if (outside_coverage) {
    "outside_nhdplus_coverage"
  } else if (is.null(last_error)) {
    "no_coverage_or_unavailable"
  } else {
    "service_unavailable"
  }
  coverage_message <- if (outside_coverage) {
    paste(
      "USGS NHDPlus does not have reach coverage at this location.",
      "No further USGS retries are needed."
    )
  } else {
    "USGS stream-network data are unavailable for this location."
  }

  if (is_usable_reach_slope(fallback_slope)) {
    return(new_reach_slope_result(
      value = fallback_slope,
      source = fallback_result$source,
      status = "fallback",
      reason = reason,
      attempts = attempts_used,
      message = paste(
        coverage_message,
        "Discharge is continuing with the positive",
        if (identical(fallback_result$source, "dem_reach")) {
          "Sampled DEM Reach slope."
        } else {
          "Local XS Neighborhood slope."
        }
      )
    ))
  }

  new_reach_slope_result(
    value = fallback_slope,
    source = fallback_result$source,
    status = "unavailable",
    reason = reason,
    attempts = attempts_used,
    message = paste(
      coverage_message,
      "Discharge is unavailable because a positive DEM slope",
      "could not be applied either.",
      "Map, cross-section, and storage results remain available."
    )
  )
}

#' Resolve the selected cross section's local DEM slope
#'
#' @noRd
resolve_dem_reach_slope <- function(xs_pts, xs_number) {
  results <- resolve_local_xs_slope_results(xs_pts)
  result <- results[[as.character(xs_number)]]

  if (is.null(result)) {
    stop("The selected cross section is not present in xs_pts")
  }

  result
}

#' Resolve and cache every Local XS Neighborhood slope in one pass
#'
#' @noRd
resolve_local_xs_slope_results <- function(xs_pts) {
  profile <- prepare_local_xs_slope_profile(xs_pts)
  results <- lapply(seq_len(nrow(profile)), function(index) {
    dem_slope <- profile$slope[[index]]
    if (is_usable_reach_slope(dem_slope)) {
      return(new_reach_slope_result(
        value = dem_slope,
        source = "dem_xs_local",
        status = "available",
        reason = NULL,
        attempts = 0L,
        message = paste(
          "The Local XS Neighborhood slope is being applied.",
          "It is calculated from adjacent cross-section thalweg elevations."
        )
      ))
    }

    new_reach_slope_result(
      value = dem_slope,
      source = "dem_xs_local",
      status = "unavailable",
      reason = "nonpositive_local_dem",
      attempts = 0L,
      message = paste(
        "The Local XS Neighborhood slope is not positive, so it cannot",
        "be applied in the Manning calculation. Choose a reach-scale slope",
        "or select another cross section."
      )
    )
  })
  names(results) <- as.character(profile$Seq)
  results
}

#' Prepare the complete Local XS Neighborhood slope profile
#'
#' @noRd
prepare_local_xs_slope_profile <- function(xs_pts) {
  xs_ss <- xs_pts %>%
    group_by(.data$Seq) %>%
    slice_min(.data$DEM_Z, n = 1, with_ties = FALSE) %>%
    rename(Z = DEM_Z) %>%
    slope_sinuosity(
      lead_n = 1,
      lag_n = 1,
      use_smoothing = FALSE,
      vert_units = "ft"
    ) %>%
    ungroup()

  xs_ss
}

#' Resolve one Sampled DEM Reach slope from longitudinal-profile points
#'
#' @noRd
resolve_sampled_dem_reach_slope <- function(flowline_pts) {
  required <- c("Z", "POINT_M")
  missing <- setdiff(required, names(flowline_pts))
  if (length(missing) > 0L) {
    stop(
      "Flowline points are missing required field(s): ",
      paste(missing, collapse = ", ")
    )
  }
  if (!is.numeric(flowline_pts$Z) || !is.numeric(flowline_pts$POINT_M)) {
    stop("Flowline point Z and POINT_M fields must be numeric.")
  }

  usable <- is.finite(flowline_pts$Z) & is.finite(flowline_pts$POINT_M)
  if (sum(usable) < 2L) {
    return(new_reach_slope_result(
      value = NA_real_,
      source = "dem_reach",
      status = "unavailable",
      reason = "insufficient_flowline_points",
      attempts = 0L,
      message = paste(
        "Sampled DEM Reach slope is unavailable because the longitudinal",
        "profile has fewer than two finite flowline points."
      )
    ))
  }

  elevations <- flowline_pts$Z[usable]
  stations_km <- flowline_pts$POINT_M[usable]
  rise_ft <- max(elevations) - min(elevations)
  run_ft <- (max(stations_km) - min(stations_km)) * 3280.84
  dem_slope <- rise_ft / run_ft

  if (is_usable_reach_slope(dem_slope)) {
    return(new_reach_slope_result(
      value = dem_slope,
      source = "dem_reach",
      status = "available",
      reason = NULL,
      attempts = 0L,
      message = paste(
        "The Sampled DEM Reach slope is being applied.",
        "It uses the minimum and maximum elevations of the flowline points",
        "shown in the longitudinal profile."
      )
    ))
  }

  new_reach_slope_result(
    value = dem_slope,
    source = "dem_reach",
    status = "unavailable",
    reason = "nonpositive_sampled_dem_reach",
    attempts = 0L,
    message = paste(
      "Sampled DEM Reach slope is unavailable because the longitudinal",
      "profile does not have a positive elevation range and length."
    )
  )
}

#' Prepare a point for USGS NHDPlus reach discovery
#'
#' @noRd
prepare_usgs_lookup_point <- function(xs_pts, xs_number) {
  selected_xs <- xs_pts %>%
    filter(.data$Seq == xs_number) %>%
    slice_min(.data$DEM_Z, n = 1, with_ties = FALSE)

  if (nrow(selected_xs) < 1L) {
    stop("The selected cross section is not present in xs_pts")
  }

  sf::st_sfc(
    sf::st_point(
      x = c(selected_xs$POINT_X[[1]], selected_xs$POINT_Y[[1]]),
      dim = "XY"
    ),
    crs = sf::st_crs(xs_pts)
  )
}

#' Retrieve a reach slope from USGS NHDPlus
#'
#' @noRd
lookup_usgs_reach_slope <- function(
  point,
  discover_fun = nhdplusTools::discover_nhdplus_id,
  subset_fun = nhdplusTools::subset_nhdplus,
  request_timeout = 8
) {
  service_call <- function() {
    start_comid <- discover_fun(
      point = point,
      nldi_feature = "comid",
      raindrop = FALSE
    )
    comid <- if (
      is.data.frame(start_comid) &&
        "comid" %in% names(start_comid) &&
        nrow(start_comid) > 0L
    ) {
      suppressWarnings(as.numeric(start_comid$comid[[1]]))
    } else if (is.atomic(start_comid) && length(start_comid) > 0L) {
      suppressWarnings(as.numeric(start_comid[[1]]))
    } else {
      NA_real_
    }

    if (!is.finite(comid)) {
      stop(new_usgs_no_coverage_condition())
    }

    output_file <- tempfile(fileext = ".gpkg")
    on.exit(unlink(output_file), add = TRUE)
    nhd_flowline <- suppressWarnings(subset_fun(
      comids = comid,
      output_file = output_file,
      nhdplus_data = "download",
      overwrite = TRUE,
      status = FALSE,
      flowline_only = TRUE
    ))
    flowlines <- nhd_flowline$NHDFlowline_Network

    if (
      is.null(flowlines) ||
        !"slope" %in% names(flowlines) ||
        nrow(flowlines) < 1L
    ) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(flowlines$slope[[1]]))
  }

  httr::with_config(
    config = httr::timeout(request_timeout),
    service_call()
  )
}

#' Construct a terminal USGS NHDPlus no-coverage condition
#'
#' @noRd
new_usgs_no_coverage_condition <- function() {
  structure(
    list(
      message = "USGS NHDPlus has no reach coverage at this location.",
      call = NULL
    ),
    class = c("usgs_no_coverage", "error", "condition")
  )
}

#' Construct a reach-slope result
#'
#' @noRd
new_reach_slope_result <- function(
  value,
  source,
  status,
  reason,
  attempts,
  message
) {
  structure(
    list(
      value = as.numeric(value[[1]]),
      source = source,
      status = status,
      reason = reason,
      attempts = as.integer(attempts),
      message = message
    ),
    class = c("reach_slope_result", "list")
  )
}

#' Check whether a slope can support a Manning calculation
#'
#' @noRd
is_usable_reach_slope <- function(value) {
  length(value) == 1L &&
    is.numeric(value) &&
    is.finite(value) &&
    value > 0
}

#' Create a discharge-unavailable table
#'
#' @noRd
discharge_unavailable_table <- function(message) {
  tibble::tibble(
    Variable = "Discharge unavailable",
    Details = message
  ) |>
    gt::gt() |>
    gt::tab_options(
      column_labels.font.weight = "bold",
      table.font.size = "small",
      column_labels.padding = gt::px(2),
      data_row.padding = gt::px(3),
      table.margin.left = gt::px(1),
      table.margin.right = gt::px(1)
    )
}

#' Prepare DEM-derived discharge values
#'
#' @param xs_pts Cross-section points.
#' @param xs_number Cross-section sequence number.
#' @param bf_estimate Relative bankfull elevation.
#' @param mannings_n Manning's roughness coefficient.
#' @param nhd_slope Reach slope.
#'
#' @return A long-form data frame of discharge values.
#' @noRd
prepare_xs_discharge_values <- function(
  xs_pts,
  xs_number,
  bf_estimate,
  mannings_n,
  nhd_slope
) {
  xs_pts_channel <- xs_pts %>%
    filter(.data$Seq == xs_number) %>%
    filter(.data$channel == 1)

  dims <- fluvgeo::xs_geometry(
    xs_points = xs_pts_channel,
    detrend_elevation = bf_estimate
  )
  drainage_area <- unique(xs_pts_channel$Watershed_Area_SqMile)
  drainage_area <- if (length(drainage_area) > 0L) {
    as.numeric(drainage_area[[1]])
  } else {
    NA_real_
  }
  nhd_slope <- as.numeric(nhd_slope[[1]])
  channel_flow <- (1.486 / mannings_n) *
    dims$xs_area *
    (dims$xs_depth^(2 / 3)) *
    (nhd_slope^(1 / 2))

  dims_table <- tibble(
    xs_area = dims$xs_area,
    xs_width = dims$xs_width,
    xs_depth = dims$xs_depth,
    drainage_area = drainage_area,
    R_proxy = dims$xs_depth,
    S_proxy = nhd_slope,
    Q = channel_flow,
    V = channel_flow / dims$xs_area
  )

  dims_table %>%
    pivot_longer(everything()) %>%
    filter(!is.na(.data$value)) %>%
    mutate(
      units = recode(
        .data$name,
        xs_area = "sq ft",
        xs_width = "ft",
        xs_depth = "ft",
        drainage_area = "sq mi",
        R_proxy = "ft",
        S_proxy = "",
        Q = "cfs",
        V = "ft sec"
      ),
      label = recode(
        .data$name,
        xs_area = "XS Area (A)",
        xs_width = "XS Width",
        xs_depth = "XS Mean Depth",
        drainage_area = "Drainage Area",
        R_proxy = "XS Hydraulic Radius (R)",
        S_proxy = "Slope (S)",
        Q = "Channel Flow (Q)",
        V = "Channel Velocity (V)"
      )
    ) %>%
    relocate("label", .before = "name") %>%
    select(-"name")
}
