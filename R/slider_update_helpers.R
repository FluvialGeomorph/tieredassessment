#' Prepare channel elevation slider update
#'
#' Captures and validates the state changes needed when channel_elevation slider
#' is moved. This helper exists so the server observer logic can be tested
#' directly without reactive machinery.
#'
#' @param channel_elevation Numeric value from the channel_elevation slider.
#' @param pick_xs Selected cross-section identifier.
#' @param xs_pts Cross-section points data frame (with required columns: Seq).
#' @param mannings_n Manning's n coefficient for discharge calculation.
#'
#' @return A list containing:
#'   - `channel_elevation_value`: captured slider value
#'   - `pick_xs`: selected cross-section
#'   - `mannings_n`: Manning's n value
#' @noRd
prepare_channel_elevation_update <- function(
  channel_elevation,
  pick_xs,
  xs_pts,
  mannings_n
) {
  stopifnot(is.numeric(channel_elevation))
  stopifnot(!is.null(xs_pts))
  stopifnot("Seq" %in% names(xs_pts))
  stopifnot(is.numeric(mannings_n))

  # Validate that the cross-section exists in xs_pts
  valid_xs <- unique(xs_pts$Seq)
  stopifnot(pick_xs %in% valid_xs)

  # Return captured values for observer to use
  list(
    channel_elevation_value = as.numeric(channel_elevation),
    pick_xs = pick_xs,
    mannings_n = as.numeric(mannings_n)
  )
}

#' Prepare floodplain elevation slider update
#'
#' Captures and validates the state changes needed when floodplain_elevation
#' slider is moved. This helper exists so the server observer logic can be
#' tested directly without reactive machinery.
#'
#' @param floodplain_elevation Numeric value from the floodplain_elevation slider.
#' @param pick_xs Selected cross-section identifier.
#' @param xs_pts Cross-section points data frame (with required columns: Seq).
#' @param mannings_n Manning's n coefficient for discharge calculation.
#'
#' @return A list containing:
#'   - `floodplain_elevation_value`: captured slider value
#'   - `pick_xs`: selected cross-section
#'   - `mannings_n`: Manning's n value
#' @noRd
prepare_floodplain_elevation_update <- function(
  floodplain_elevation,
  pick_xs,
  xs_pts,
  mannings_n
) {
  stopifnot(is.numeric(floodplain_elevation))
  stopifnot(!is.null(xs_pts))
  stopifnot("Seq" %in% names(xs_pts))
  stopifnot(is.numeric(mannings_n))

  # Validate that the cross-section exists in xs_pts
  valid_xs <- unique(xs_pts$Seq)
  stopifnot(pick_xs %in% valid_xs)

  # Return captured values for observer to use
  list(
    floodplain_elevation_value = as.numeric(floodplain_elevation),
    pick_xs = pick_xs,
    mannings_n = as.numeric(mannings_n)
  )
}

#' Validate slider update safety
#'
#' Checks that slider value is within acceptable bounds for the selected
#' cross-section. This helps catch cases where a slider update might produce
#' invalid state.
#'
#' @param elevation_value Numeric slider value.
#' @param xs_pts Cross-section points data frame (with Detrend_DEM_Z column).
#' @param pick_xs Selected cross-section identifier (Seq value).
#'
#' @return Logical: TRUE if the elevation is within bounds, FALSE otherwise.
#' @noRd
is_elevation_value_valid <- function(elevation_value, xs_pts, pick_xs) {
  if (!is.numeric(elevation_value)) return(FALSE)
  if (is.null(xs_pts)) return(FALSE)
  if (!("Seq" %in% names(xs_pts))) return(FALSE)
  if (!("Detrend_DEM_Z" %in% names(xs_pts))) return(FALSE)

  xs_data <- xs_pts[xs_pts$Seq == pick_xs, ]
  if (nrow(xs_data) == 0) return(FALSE)

  dem_z_vals <- xs_data$Detrend_DEM_Z
  rem_min <- round(min(dem_z_vals), 1) + 0.1
  rem_max <- round(max(dem_z_vals), 0) - 1

  # Value should be within computed bounds
  elevation_value >= rem_min && elevation_value <= rem_max
}
