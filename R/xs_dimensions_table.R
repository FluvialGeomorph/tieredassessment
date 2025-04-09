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
#' @return a `gtable` object
#'
#' @importFrom dplyr filter .data distinct select mutate across recode arrange
#' @importFrom gt gt cols_label_with
#'
xs_dimensions_table <- function(xs_pts, xs_number, bf_estimate, regions) {
  
  # Get the channel portion of the current cross section
  xs_pts_channel <- xs_pts %>%
    filter(.data$Seq == xs_number) %>%
    filter(.data$channel == 1)
  
  # Calculate channel dimensions
  dims <- fluvgeo::xs_dimensions(xs_points = xs_pts_channel,
                                 streams = unique(xs_pts_channel$ReachName),
                                 regions = regions,
                                 bankfull_elevations = bf_estimate)
  # Wrangle the dimensions
  dims_table <- dims %>%
    distinct() %>%
    select(-c("reach_name", "cross_section", "bankfull_elevation",
              "discharge")) %>%
    mutate(across(2:5, \(x) round(x, 1))) %>%
    mutate(xs_type = recode(xs_type,
                            "DEM derived cross section" = "DEM derived")) %>%
    arrange(.data$xs_type) %>%
    arrange(match(xs_type, c("DEM derived"))) %>%
    filter(xs_type == "DEM derived") %>%
    select(-drainage_area)
  
  gt_table <- 
    dims_table |>
    gt::gt() |>
    gt::cols_label_with(fn = function(x) tools::toTitleCase(gsub("_", " ", x)))
    
  return(dims_table)
}