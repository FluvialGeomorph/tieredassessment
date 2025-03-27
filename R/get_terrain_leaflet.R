#' Get a Terrain Leaflet
#' 
#' @description Get a terrain map as a leaflet object. 
#'
#' @param xs        sf object, The cross sections
#' @param dem       SpatRaster object, The digital elevation model. 
#'
#' @return a leaflet object
#' @export
#' 
#' @importFrom leaflet colorNumeric addRasterImage addLegend addProviderTiles
#'                     addLayersControl
#' @importFrom leaflegend addLegendNumeric
#' @importFrom fluvgeo map_extent
#' @importFrom assertthat assert_that
#' @importFrom sf st_as_sf st_as_sfc
#' @importFrom terra crop minmax values
#'
get_terrain_leaflet <- function(xs, dem) {
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
  
  # Create a topo color mapping
  leaflet_topo <- colorNumeric(
    palette = c("cadetblue2", "khaki1", "chartreuse4", "goldenrod1", 
                "orangered4", "saddlebrown", "gray70", "white"),
    domain = as.vector(minmax(dem_i)),
    na.color = "transparent",
    reverse = FALSE)
  
  terrain_leaflet <- leaflet() %>%
    addRasterImage(dem_i,
                   colors = leaflet_topo,
                   group = "Elevation",
                   opacity = 0.7, project = FALSE) %>%
    addLegendNumeric(pal = leaflet_topo,
                     values = values(dem_i),
                     title = "NAVD88, ft",
                     decreasing = TRUE,
                     group = "Elevation",
                     position = "topright") %>%
    addProviderTiles("USGS.USTopo", group = "USGS Topo") %>%
    addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
    addLayersControl(
      baseGroups = c("USGS Topo", "Imagery"),
      overlayGroups = c("Elevation"),
      position = "topleft")
  
  #terrain_leaflet
  return(terrain_leaflet)
}

# terrain_tmap <- 
#   tm_shape(shp = dem_i,
#            name = "DEM",
#            unit = "ft",
#            is.main = FALSE) +
#     tm_raster(group = "Elevation",
#               zindex = 402,
#               col.scale = tm_scale_continuous(values = "brewer.rd_yl_gn"),
#               col_alpha = 1,
#               col.legend = tm_legend(
#                 title = "NAVD88, ft",
#                 reverse = FALSE,
#                 frame = FALSE,
#                 position = legend_pos)) +
#   tm_shape(shp = xs,
#            name = "XS",
#            is.main = TRUE,
#            bbox = xs_extent) + 
#     tm_lines(group = "Cross Section",
#              zindex = 403,
#              col = "grey50",
#              col_alpha = 1,
#              lwd = 7) + 
#     tm_text(group = "Cross Section",
#             zindex = 404,
#             text = "Seq",
#             col = "black",
#             size = 1.1,
#             fontface = "bold",
#             options = opt_tm_text(remove_overlap = FALSE,
#                                   shadow = FALSE)) +
#   tm_layout(meta.margins = c(0, 0, 0, 0.15)) 
# terrain_tmap