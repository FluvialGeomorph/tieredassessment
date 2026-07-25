#' @title Create a Cross Section Dimensions Table
#'
#' @description
#' Creates a cross section dimensions table for the channel portion of the
#' specified cross section.
#'
#' @export
#' @param xs_pts        sf; A cross section lines feature class.
#' @param xs_number     integer; The cross section `Seq` number of the
#'                      requested cross section.
#' @param bf_estimate   numeric; Detrended bankfull estimate (units:
#'                      detrended feet).
#' @param regions       Deprecated compatibility parameter. Regional-curve
#'                      dimensions are not needed for this DEM-derived table.
#'
#' @return a `gt` object
#'
#' @importFrom fluvgeo xs_geometry
#' @importFrom dplyr filter .data select
#' @importFrom gt gt fmt_number cols_label tab_options px
#'
xs_dimensions_table <- function(xs_pts, xs_number, bf_estimate, regions) {
  # Get the channel portion of the current cross section
  xs_pts_channel <- xs_pts %>%
    filter(.data$Seq == xs_number) %>%
    filter(.data$channel == 1)
  
  # Calculate only the DEM-derived geometry required by this table. Regional
  # curves require drainage area, but this table does not.
  dims_table <- fluvgeo::xs_geometry(
    xs_points = xs_pts_channel,
    detrend_elevation = bf_estimate
  ) %>%
    select(xs_area, xs_width, xs_depth)
  
  gt_table <- dims_table |>
    gt() |>
    cols_label(xs_area = "Area (sq_ft)") |>
    cols_label(xs_width = "Width (ft)") |>
    cols_label(xs_depth = "Mean Depth (ft)") |>
    fmt_number(columns = everything(), decimals = 1) |>
    tab_options(
      column_labels.font.weight = "bold",
      table.font.size = "small",
      column_labels.padding = px(2),
      data_row.padding = px(1))
  #gt_table
  return(gt_table)
}
