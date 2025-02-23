#' Get a Terrain Map
#' 
#' @description Get a terrain map as a tmap object. 
#'
#' @param xs        sf object, The cross sections
#' @param dem       SpatRaster object, The digital elevation model. 
#'
#' @return a tmap object
#' @export
#'
#' @importFrom tmap tmap_mode tm_shape tm_raster tm_lines tm_pos_out tm_text
#'                  opt_tm_text tm_scale_continuous tm_legend tm_layout 
#' @importFrom grDevices colorRampPalette
#' @importFrom fluvgeo map_extent
#' @importFrom assertthat assert_that
#' @importFrom sf st_as_sf st_as_sfc
#' @importFrom terra crop terrain shade
#'
get_terrain_map <- function(xs, dem) {
  assert_that("sf" %in% class(xs), 
               msg = "must be sf object")
  assert_that("SpatRaster" %in% class(dem), 
              msg = "must be SpatRaster object")
  
  # set extent
  xs_extent <- fluvgeo::map_extent(xs, extent_factor = 1.2)
  xs_extent_poly <- sf::st_as_sf(sf::st_as_sfc(xs_extent))
  
  # Crop the dem to the cross section map extent (+ 20 pixels)
  dem_i <- terra::crop(x = dem,
                       y = terra::ext(xs_extent_poly) + 20)
  
  # Create a hillshade from dem_i
  slp <- terra::terrain(dem_i, v = "slope", unit = "radians")
  asp <- terra::terrain(dem_i, v = "aspect", unit = "radians")
  hill <- terra::shade(slope = slp, aspect = asp)
  
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
    tm_shape(shp = hill,
             name = "Hillshade",
             is.main = FALSE,
             zindex = 401) +
      tm_raster(group = "Elevation",
                col.scale = tm_scale_continuous(values = "hcl.grays"),
                col.legend = tm_legend(show = FALSE)) +
    tm_shape(shp = dem_i,
             name = "DEM",
             unit = "ft",
             is.main = FALSE,
             zindex = 402) +
      tm_raster(group = "Elevation",
                col.scale = tm_scale_continuous(values = esri_topo(1000)),
                col_alpha = 0.8,
                col.legend = tm_legend(
                  title = "NAVD88, ft",
                  reverse = TRUE,
                  frame = FALSE,
                  position = legend_pos)) +
    tm_shape(shp = xs,
             name = "XS",
             is.main = TRUE,
             bbox = xs_extent,
             zindex = 403) + 
      tm_lines(group = "Cross Section",
               col = "grey50",
               col_alpha = 0.6,
               lwd = 7) + 
      tm_text(text = "Seq",
              col = "black",
              size = 1.1,
              fontface = "bold",
              options = opt_tm_text(remove_overlap = FALSE,
                                    shadow = FALSE)) +
    tm_layout(meta.margins = c(0, 0, 0, 0.15))
  
  terrain_map
  return(terrain_map)
}