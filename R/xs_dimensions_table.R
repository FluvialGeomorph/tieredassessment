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
#' @param regions       character vector; Regions to calculate hydraulic
#'                      dimensions for. See the `RegionalCurve` package for
#'                      a list of regions.
#'
#' @return a `gt` object
#'
#' @importFrom fluvgeo xs_dimensions
#' @importFrom dplyr filter .data distinct select mutate recode across arrange
#' @importFrom gt gt fmt_number cols_label tab_options px
#'
xs_dimensions_table <- function(xs_pts, xs_number, bf_estimate, regions) {
  # Get the channel portion of the current cross section
  xs_pts_channel <- xs_pts %>%
    filter(.data$Seq == xs_number) %>%
    filter(.data$channel == 1)
  
  # Calculate channel dimensions
  dims <- fluvgeo::xs_dimensions(
    xs_points = xs_pts_channel,
    streams = unique(xs_pts_channel$ReachName),
    regions = regions,
    bankfull_elevations = bf_estimate
  )
  # Wrangle the dimensions
  dims_table <- dims %>%
    distinct() %>%
    select(-c(
      "reach_name",
      "cross_section",
      "bankfull_elevation",
      "discharge"
    )) %>%
    mutate(xs_type = recode(.data$xs_type, 
                            "DEM derived cross section" = "DEM derived")) %>%
    arrange(.data$xs_type) %>%
    arrange(match(.data$xs_type, c("DEM derived"))) %>%
    filter(.data$xs_type == "DEM derived") %>%
    select(-c(drainage_area, xs_type))
  
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
