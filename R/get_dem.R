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
#' @importFrom sf st_transform st_bbox
#' 
get_dem <- function(xs) {
  # authenticate to AGOL
  tieredassessment:::arcgis_auth()
  
  dem_url <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  dem_service  <- arc_open(dem_url)
  dem_crs <- dem_service$spatialReference$latestWkid
  # ensure xs is in the same crs as dem
  xs_dem  <- sf::st_transform(xs, dem_crs)
  xs_bbox <- fluvgeo::map_extent(xs_dem, extent_factor = 1.2)

  dem <- arc_raster(dem_service, 
                    # only get the extent of the xs
                    xmin = xs_bbox$xmin, 
                    ymin = xs_bbox$ymin,
                    xmax = xs_bbox$xmax,
                    ymax = xs_bbox$ymax)
  
  # Convert elevations from meters to feet
  dem_m <- dem * 3.28084
  
  return(dem_m)
}  

