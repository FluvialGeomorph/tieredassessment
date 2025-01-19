#' Get DEM
#' 
#' @description Get a DEM from the ESRI WorldElevation Terrain Image Service.
#'
#' @param xs      sf object
#'
#' @return terra::SpatRaster
#'
#' @importFrom httr2 oauth_token
#' @importFrom arcgisutils set_arc_token
#' @importFrom arcgislayers arc_open arc_raster
#' @importFrom sf st_transform st_bbox
#' 
#' 
get_dem <- function(xs) {
  url <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  
  # Authorize with ESRI
  # https://usace-mvr.maps.arcgis.com/home/item.html?id=b5e3ddc1fbb444cda8d5837693e45739
  temp <- "3NKHt6i2urmWtqOuugvr9SyZ2rLHIP9Ju0rmtGyg8pEyNzxKA_WbVzdK86poz-qs03NQKBTyY9cFPl-dOJt_1GbIhWOXn0pe5qG-1_JLbhmjyFgfLWt0pf0dhwrk4RgQ"
  token <- httr2::oauth_token(temp,
                              arcgis_host = "https://usace-mvr.maps.arcgis.com/")
  set_arc_token(token)
  
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