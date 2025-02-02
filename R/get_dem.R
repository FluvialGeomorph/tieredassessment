#' Get DEM
#' 
#' @description Get a DEM from the ESRI WorldElevation Terrain Image Service 
#' that covers the extent of the input features.
#'
#' @param xs      sf object
#'
#' @return terra::SpatRaster
#'
#' @importFrom arcgislayers arc_open arc_raster
#' @importFrom sf st_transform st_bbox
#' 
get_dem <- function(xs) {
  # authenticate to AGOL
  arcgis_auth()
  
  url <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  imgsrv  <- arc_open(url)
  dem_crs <- imgsrv$spatialReference$latestWkid
  xs_dem  <- sf::st_transform(xs, dem_crs)
  xs_bbox <- sf::st_bbox(xs_dem)

  dem <- arc_raster(imgsrv, 
                    xmin = xs_bbox$xmin, 
                    ymin = xs_bbox$ymin,
                    xmax = xs_bbox$xmax,
                    ymax = xs_bbox$ymax)
  return(dem)
}  

