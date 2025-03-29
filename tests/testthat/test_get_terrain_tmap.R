# R 4.4.0 has bug that cannot display js-based htmlwidgets in viewer

test_that("map is a tmap, from geojson, plot mode", {
  xs <- sf::st_read(system.file("extdata", "xs.geojson", 
                                package = "tieredassessment"), quiet = TRUE)
  new_xs <- sf::st_transform(xs, crs = 3857) # Web Mercator
  dem <- get_dem(new_xs)
  tmap::tmap_mode("plot")
  map_plot <- get_terrain_map(new_xs, dem)
  map_plot
  expect_true("tmap" %in% class(map_plot))
})

test_that("map is a tmap, from shapefile", {
  xs <- sf::st_read(system.file("extdata", "xs.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  new_xs <- sf::st_transform(xs, crs = 3857) # Web Mercator
  dem <- get_dem(new_xs)
  tmap::tmap_mode("view")
  map <- get_terrain_map(new_xs, dem)
  map
  expect_true("tmap" %in% class(map))
})

test_that("can it be converted to leaflet", {
  xs <- sf::st_read(system.file("extdata", "xs.geojson", 
                                package = "tieredassessment"), quiet = TRUE)
  new_xs <- sf::st_transform(xs, crs = 3857) # Web Mercator
  dem <- get_dem(new_xs)
  map <- get_terrain_map(new_xs, dem)
  tmap::tmap_mode("view")
  lf <- tmap::tmap_leaflet(map, show = TRUE)
  expect_true("leaflet" %in% class(lf))
  terrain_map <-
    tmap_leaflet(map, in.shiny = TRUE) %>%
    addProviderTiles("USGS.USTopo", group = "USGS Topo") %>%
    addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
    addLayersControl(
      baseGroups = c("USGS Topo", "Imagery"),
      overlayGroups = c("Elevation", "Cross Section"),
      position = "topleft")
  terrain_map
})