#' Get a Terrain Map
#' 
#' @description Get a terrain map as a tmap object.#' 
#'
#' @param xs        sf object, The cross sections
#' @param dem       SpatRaster object, The digital elevation model. 
#'
#' @return a tmap object
#' @export
#'
#' @importFrom tmap tmap_mode tm_shape tm_raster tm_lines tm_pos_out 
#'                  tm_scale_continuous tm_legend tm_layout 
#' @importFrom grDevices colorRampPalette
#' @importFrom fluvgeo map_extent
#'
get_terrain_map <- function(xs, dem) {
  assertthat::assert_that("sf" %in% class(xs), 
                          msg = "must be sf object")
  assertthat::assert_that("SpatRaster" %in% class(dem), 
                          msg = "must be SpatRaster object")
  
  # set extent
  xs_extent <- fluvgeo::map_extent(xs, extent_factor = 1.2)
  
  # Create a topo color ramp
  esri_topo <- grDevices::colorRampPalette(colors = c("cadetblue2", "khaki1",
                                                      "chartreuse4", "goldenrod1",
                                                      "orangered4", "saddlebrown",
                                                      "gray70", "white"),
                                           bias = 1,
                                           space = "Lab",
                                           interpolate = "linear")
  # Specify legend position
  legend_pos <- tm_pos_out(cell.h = "right",
                           cell.v = "center",
                           pos.v = "top",
                           pos.h = "left")
  
  terrain_map <- 
    tm_shape(shp = dem,
             id = "Elevation",
             unit = "ft",
             zindex = 401) +
      tm_raster(col.scale = tm_scale_continuous(values = esri_topo(1000)),
                col.legend = tm_legend(
                  title = "Elevation \n(NAVD88, ft)",
                  reverse = TRUE,
                  frame = FALSE,
                  position = legend_pos)) +
    tm_shape(shp = xs,
             id = "Cross Section",
             is.main = TRUE,
             bbox = xs_extent,
             zindex = 402) + 
      tm_lines() + 
    tm_layout(meta.margins = c(0, 0, 0, 0.15))
  
  #terrain_map
  return(terrain_map)
}