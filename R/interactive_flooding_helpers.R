#' Prepare an exact lookup for repeated floodplain-volume calculations
#'
#' The lookup preserves the calculation performed by
#' `fluvgeo::floodplain_volume()` for water surfaces expressed as
#' `trend + (REM elevation - 100)`.
#'
#' @noRd
prepare_floodplain_volume_lookup <- function(dem, trend) {
  if (!inherits(dem, "SpatRaster") || !inherits(trend, "SpatRaster")) {
    stop("dem and trend must be SpatRaster objects.")
  }

  dem_resampled <- terra::resample(dem, trend, method = "bilinear")
  dem_values <- as.numeric(terra::values(dem_resampled, mat = FALSE))
  trend_values <- as.numeric(terra::values(trend, mat = FALSE))
  usable <- is.finite(dem_values) & is.finite(trend_values)
  thresholds <- sort(dem_values[usable] - trend_values[usable] + 100)
  cell_area_m2 <- mean(as.numeric(terra::values(
    terra::cellSize(dem, unit = "m"),
    mat = FALSE
  )))

  structure(
    list(
      thresholds = thresholds,
      threshold_cumsum = cumsum(thresholds),
      cell_area_m2 = cell_area_m2
    ),
    class = c("floodplain_volume_lookup", "list")
  )
}

#' Calculate floodplain volume from a prepared lookup
#'
#' @noRd
calculate_floodplain_volume <- function(lookup, rem_elevation) {
  if (!inherits(lookup, "floodplain_volume_lookup")) {
    stop("lookup must be a floodplain_volume_lookup.")
  }
  if (
    !is.numeric(rem_elevation) ||
      length(rem_elevation) != 1L ||
      !is.finite(rem_elevation)
  ) {
    stop("rem_elevation must be one finite numeric value.")
  }

  count <- findInterval(rem_elevation, lookup$thresholds)
  if (count < 1L || !is.finite(lookup$cell_area_m2)) {
    return(0)
  }

  depth_sum_ft <- (
    count * rem_elevation -
      lookup$threshold_cumsum[[count]]
  )
  as.numeric(depth_sum_ft * 0.3048 * lookup$cell_area_m2)
}

#' Update one polygon-membership field on cross-section points
#'
#' @noRd
update_xs_polygon_classification <- function(
  xs_pts,
  polygon,
  field,
  buffer_distance
) {
  if (!inherits(xs_pts, "sf") || !inherits(polygon, "sf")) {
    stop("xs_pts and polygon must be sf objects.")
  }
  if (
    !is.character(field) ||
      length(field) != 1L ||
      !field %in% c("channel", "floodplain")
  ) {
    stop("field must be either 'channel' or 'floodplain'.")
  }
  if (
    !is.numeric(buffer_distance) ||
      length(buffer_distance) != 1L ||
      !is.finite(buffer_distance)
  ) {
    stop("buffer_distance must be one finite numeric value.")
  }

  polygon_buffer <- terra::buffer(
    terra::vect(polygon),
    width = buffer_distance
  )
  point_hits <- terra::relate(
    terra::vect(xs_pts),
    polygon_buffer,
    "intersects"
  )
  xs_pts[[field]] <- ifelse(as.vector(point_hits), 1, 0)
  xs_pts
}

#' Construct an empty bounded water-surface polygon cache
#'
#' @noRd
new_water_surface_polygon_cache <- function(max_entries = 32L) {
  max_entries <- as.integer(max_entries)
  if (
    length(max_entries) != 1L ||
      is.na(max_entries) ||
      max_entries < 1L
  ) {
    stop("max_entries must be a positive integer.")
  }

  structure(
    list(
      max_entries = max_entries,
      entries = list(),
      recency = character()
    ),
    class = c("water_surface_polygon_cache", "list")
  )
}

#' Resolve a water-surface polygon through a bounded LRU cache
#'
#' @noRd
resolve_cached_water_surface_polygon <- function(
  cache,
  rem_elevation,
  rem,
  flowline,
  resolver = fluvgeo::water_surface_poly
) {
  if (!inherits(cache, "water_surface_polygon_cache")) {
    stop("cache must be a water_surface_polygon_cache.")
  }
  if (
    !is.numeric(rem_elevation) ||
      length(rem_elevation) != 1L ||
      !is.finite(rem_elevation)
  ) {
    stop("rem_elevation must be one finite numeric value.")
  }

  key <- paste0("rem_", formatC(
    rem_elevation,
    format = "f",
    digits = 6
  ))
  if (key %in% names(cache$entries)) {
    cache$recency <- c(setdiff(cache$recency, key), key)
    return(list(
      polygon = cache$entries[[key]],
      cache = cache,
      cache_hit = TRUE,
      key = key
    ))
  }

  polygon <- resolver(
    rem = rem,
    water_surface_elevation = rem_elevation,
    flowline = flowline
  )
  cache$entries[[key]] <- polygon
  cache$recency <- c(setdiff(cache$recency, key), key)

  while (length(cache$recency) > cache$max_entries) {
    evicted <- cache$recency[[1]]
    cache$entries[[evicted]] <- NULL
    cache$recency <- cache$recency[-1]
  }

  list(
    polygon = polygon,
    cache = cache,
    cache_hit = FALSE,
    key = key
  )
}
