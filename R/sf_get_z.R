#' @title SF Get Z
#' @description Adds a new attribute "Z" to the input points sf object based
#' on the input dem.
#' @param points
#' @param dem
#'
#' @returns
#' @export
#'
#' @importFrom fluvgeo sf_point_attributes
#' @importFrom terra vect extract
#' 
sf_get_z <- function(points, dem) {
  pts <- terra::vect(points)
  
  pts_z <- terra::extract(x = pts, y = dem)
  return(pts_z)
}