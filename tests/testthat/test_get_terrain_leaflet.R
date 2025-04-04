# R 4.4.0 has bug that cannot display js-based htmlwidgets in viewer

test_that("leaflet from geojson", {
  xs_geojson <- sf::st_read(system.file("extdata", "xs.geojson", 
                            package = "tieredassessment"), quiet = TRUE)
  xs <- sf::st_transform(xs_geojson, crs = 3857) # Web Mercator
  xs$Seq <- row.names(xs)
  dem <- get_dem(xs)
  leaf <- get_terrain_leaflet(xs, dem)
  leaf
  expect_true("leaflet" %in% class(leaf))
})

test_that("leaflet from shapefile", {
  xs_shp <- sf::st_read(system.file("extdata", "xs.shp", 
                        package = "tieredassessment"), quiet = TRUE)
  xs <- sf::st_transform(xs_shp, crs = 3857) # Web Mercator
  xs$Seq <- row.names(xs)
  dem <- get_dem(xs)
  leaf <- get_terrain_leaflet(xs, dem)
  leaf
  expect_true("leaflet" %in% class(leaf))
})
