#' Prepare Results slider state for update
#'
#' Captures the current slider values and computes the slider range used during
#' Results initialization.
#'
#' @param xs_pts A data frame-like object containing at least `Seq` and
#'   `Detrend_DEM_Z` columns.
#' @param pick_xs The selected cross-section identifier.
#' @param channel_elevation Current channel elevation slider value.
#' @param floodplain_elevation Current floodplain elevation slider value.
#'
#' @return A list with:
#'   - `rem_min`
#'   - `rem_max`
#'   - `channel_elevation_value`
#'   - `floodplain_elevation_value`
#' @noRd
prepare_results_slider_state <- function(
  xs_pts,
  pick_xs,
  channel_elevation,
  floodplain_elevation
) {
  stopifnot(!is.null(xs_pts))
  stopifnot("Seq" %in% names(xs_pts))
  stopifnot("Detrend_DEM_Z" %in% names(xs_pts))

  rem_min <- round(
    min(dplyr::filter(xs_pts, Seq == as.numeric(pick_xs))$Detrend_DEM_Z),
    1
  ) + 0.1
  rem_min <- ifelse(rem_min > 100, rem_min, 100)

  rem_max <- round(
    max(dplyr::filter(xs_pts, Seq == as.numeric(pick_xs))$Detrend_DEM_Z),
    0
  ) - 1

  list(
    rem_min = rem_min,
    rem_max = rem_max,
    channel_elevation_value = channel_elevation,
    floodplain_elevation_value = floodplain_elevation
  )
}

#' Prepare Results workflow state
#'
#' This helper captures the workflow values needed to transition the app into
#' the Results state in a testable way.
#'
#' @param xs_pts Data frame-like object with `Seq` and `Detrend_DEM_Z`.
#' @param pick_xs Selected cross-section identifier.
#' @param channel_elevation Current channel elevation slider value.
#' @param floodplain_elevation Current floodplain elevation slider value.
#'
#' @return A list containing:
#'   - `slider_state`: output from `prepare_results_slider_state()`
#'   - `results_loaded`: always `TRUE` for a successful transition
#' @noRd
prepare_results_workflow_state <- function(
  xs_pts,
  pick_xs,
  channel_elevation,
  floodplain_elevation
) {
  slider_state <- prepare_results_slider_state(
    xs_pts = xs_pts,
    pick_xs = pick_xs,
    channel_elevation = channel_elevation,
    floodplain_elevation = floodplain_elevation
  )

  list(
    slider_state = slider_state,
    results_loaded = TRUE
  )
}
