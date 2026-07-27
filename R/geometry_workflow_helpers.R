#' Prepare a cross-section editor snapshot
#'
#' @param geometry Current `sf` geometry returned by the Draw XS editor.
#'
#' @return Cross-section linework in EPSG:3857 with stable sequential `Seq`
#'   identifiers.
#' @noRd
prepare_cross_section_geometry_snapshot <- function(geometry) {
  validate_editor_linework(geometry, "cross section")

  snapshot <- sf::st_transform(geometry, crs = 3857)
  snapshot[["Seq"]] <- seq_len(nrow(snapshot))
  snapshot[, c("Seq", attr(snapshot, "sf_column")), drop = FALSE]
}

#' Prepare a flowline editor snapshot
#'
#' The most recently drawn flowline is the active line when more than one
#' remains in the editor.
#'
#' @param geometry Current `sf` geometry returned by the Draw Flowline editor.
#'
#' @return One flowline in EPSG:3857.
#' @noRd
prepare_flowline_geometry_snapshot <- function(geometry) {
  validate_editor_linework(geometry, "flowline")
  geometry <- geometry[nrow(geometry), , drop = FALSE]
  sf::st_transform(geometry, crs = 3857)
}

#' @noRd
validate_editor_linework <- function(geometry, label) {
  if (!inherits(geometry, "sf") || nrow(geometry) == 0L) {
    stop("Draw at least one ", label, " before continuing.", call. = FALSE)
  }
  if (is.na(sf::st_crs(geometry))) {
    stop("The ", label, " geometry has no coordinate system.", call. = FALSE)
  }
  if (any(sf::st_is_empty(geometry))) {
    stop("Remove or redraw empty ", label, " geometry.", call. = FALSE)
  }

  geometry_types <- as.character(sf::st_geometry_type(geometry))
  if (any(!geometry_types %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("Only line geometry can be used as a ", label, ".", call. = FALSE)
  }

  invisible(geometry)
}

#' Preflight a DEM request
#'
#' Applies deterministic application limits before contacting the terrain
#' service. Service-side coverage and availability are validated separately
#' after the request.
#'
#' @param xs Cross-section snapshot in a projected CRS.
#' @param max_span_m Maximum buffered request span in metres.
#' @param extent_factor Expansion factor used by `fluvgeo::get_dem()`.
#'
#' @return A structured preflight result.
#' @noRd
prepare_dem_request_preflight <- function(
  xs,
  max_span_m = 10000,
  extent_factor = 1.5
) {
  if (!inherits(xs, "sf") || nrow(xs) == 0L) {
    stop("`xs` must contain cross-section geometry.", call. = FALSE)
  }
  if (!is.numeric(max_span_m) ||
      length(max_span_m) != 1L ||
      !is.finite(max_span_m) ||
      max_span_m <= 0) {
    stop("`max_span_m` must be one positive finite number.", call. = FALSE)
  }

  geographic <- sf::st_transform(xs, crs = 4326)
  geographic_bbox <- sf::st_bbox(geographic)
  within_web_mercator <- all(is.finite(geographic_bbox)) &&
    geographic_bbox[["xmin"]] >= -180 &&
    geographic_bbox[["xmax"]] <= 180 &&
    geographic_bbox[["ymin"]] >= -85.051129 &&
    geographic_bbox[["ymax"]] <= 85.051129

  if (!within_web_mercator) {
    return(list(
      ok = FALSE,
      reason = "out_of_bounds",
      request_span_m = NA_real_,
      max_span_m = max_span_m,
      message = paste(
        "The drawn cross sections are outside the supported terrain map",
        "extent. Move the site within the displayed world map and try again."
      )
    ))
  }

  projected <- sf::st_transform(xs, crs = 3857)
  projected_bbox <- sf::st_bbox(projected)
  width_m <- unname(projected_bbox[["xmax"]] - projected_bbox[["xmin"]])
  height_m <- unname(projected_bbox[["ymax"]] - projected_bbox[["ymin"]])
  request_span_m <- max(width_m, height_m) * extent_factor

  if (!is.finite(request_span_m) || request_span_m <= 0) {
    return(list(
      ok = FALSE,
      reason = "invalid_extent",
      request_span_m = request_span_m,
      max_span_m = max_span_m,
      message = paste(
        "The drawn cross sections do not define a valid terrain request.",
        "Adjust or redraw them and try again."
      )
    ))
  }

  if (request_span_m > max_span_m) {
    return(list(
      ok = FALSE,
      reason = "too_large",
      request_span_m = request_span_m,
      max_span_m = max_span_m,
      message = paste0(
        "The buffered terrain request is about ",
        format(round(request_span_m / 1000, 1), nsmall = 1),
        " km across; this small-site app allows up to ",
        format(round(max_span_m / 1000, 1), nsmall = 1),
        " km. Move the cross sections closer together and try again."
      )
    ))
  }

  list(
    ok = TRUE,
    reason = NULL,
    request_span_m = request_span_m,
    max_span_m = max_span_m,
    message = NULL
  )
}

#' Classify a DEM service error for user-facing handling
#'
#' @param error An error condition.
#'
#' @return A list containing `reason` and nontechnical `message`.
#' @noRd
classify_dem_request_error <- function(error) {
  detail <- tolower(conditionMessage(error))

  if (grepl(
    "outside|out of bounds|no data|no raster|empty|extent",
    detail
  )) {
    return(list(
      reason = "no_coverage",
      message = paste(
        "Terrain data are not available for the drawn site.",
        "Move the cross sections to a covered area and try again."
      )
    ))
  }

  list(
    reason = "service_unavailable",
    message = paste(
      "The terrain service could not complete the request.",
      "Your cross sections remain available; wait a moment and try",
      "Draw Flowline again."
    )
  )
}

#' Validate a returned DEM
#'
#' @param dem Candidate terrain raster.
#'
#' @return `TRUE` when at least one finite elevation is available.
#' @noRd
dem_has_finite_elevations <- function(dem) {
  inherits(dem, "SpatRaster") &&
    terra::ncell(dem) > 0L &&
    terra::hasValues(dem) &&
    any(is.finite(terra::values(dem, mat = FALSE)))
}
