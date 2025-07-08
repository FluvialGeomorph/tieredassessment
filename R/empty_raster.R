#' @title Empty terra SpatRaster object
#' @description Returns an empty terra SpatRaster object.
#' @returns An empty terra SpatRaster object
#' @importFrom terra rast crs
empty_raster <- function() {
  raster <- matrix(1:25, nrow = 5, ncol = 5) %>%
    terra::rast()
  terra::crs(raster) <- "EPSG:3857"
  return(raster)
}
