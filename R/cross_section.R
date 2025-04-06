#' @title Cross Section
#' @description Creates a valid cross section feature. 
#' @param xs               sf object; A newly digitized set of cross sections. 
#' @param flowline_points  sf object; A flowline_points feature. 
#'
#' @returns an sf object representing a set of cross sections
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_crs
#' 
cross_section <- function(xs, flowline_points) {
  assert_that("sf" %in% class(xs), 
              msg = "xs must be sf object")
  assert_that("sf" %in% class(flowline_points), 
              msg = "flowline_points must be sf object")
  assert_that(st_crs(xs) == st_crs(flowline_points), 
              msg = "xs and flowline_points must be have the same crs")
  
  # Set river position (check step: river_position)
  ## add fields POINT_X, POINT_Y, POINT_M, Z, km_to_mouth and calc from nearest fl_pt
  
  # Set sequence number (check step: assign_ids)
  ## add field `Seq` and calculate
  
  # Set watershed area (check step: watershed_area)
  ## add field `Watershed_Area_SqMile` and calculate
  
  # Set m-values for each xs (check step: station_points)
  ## add fields from_measure, to_measure and calculate
  
}