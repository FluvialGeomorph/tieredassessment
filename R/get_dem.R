#' Get DEM
#' 
#' @description Get a DEM from the ESRI WorldElevation Terrain Image Service 
#' that covers the extent of the input features.
#'
#' @param xs      sf object
#'
#' @return terra::SpatRaster
#' @export
#'
#' @importFrom arcgislayers arc_open arc_raster
#' @importFrom sf st_transform st_crs
#' @importFrom terra project
#' @importFrom assertthat assert_that
get_dem <- function(xs) {
  assert_that(check_crs_3857(xs), msg = "xs CRS must be 3857")
  
  # authenticate to AGOL
  tieredassessment:::arcgis_auth()
  
  # Define the terrain service
  dem_url <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  dem_service  <- arc_open(dem_url)
  dem_crs <- dem_service$spatialReference$latestWkid
  # Transform xs to the crs of the dem to get the bbox in the dem crs
  xs_dem  <- st_transform(xs, crs = dem_crs)
  xs_dem
  xs_bbox <- fluvgeo::map_extent(xs_dem, extent_factor = 1.2)
  xs_bbox

  dem <- arc_raster(dem_service, 
                    # only get the extent of the xs
                    xmin = xs_bbox$xmin, 
                    ymin = xs_bbox$ymin,
                    xmax = xs_bbox$xmax,
                    ymax = xs_bbox$ymax)
  
  # Convert elevations from meters to feet
  dem_m <- dem * 3.28084
  
  # Transform the dem to the xs crs (3857)
  dem_3857 <- project(dem_m, "EPSG:3857")
  assert_that(check_crs_3857(dem_3857), msg = "output dem CRS must be 3857")
  
  return(dem_3857)
}  

