test_that("map is a tmap", {
  xs <- sf::st_read(system.file("extdata", "xs.geojson", 
                                package = "tieredassessment"), quiet = TRUE)
  dem <- get_dem(xs)
  map <- get_terrain_map(xs, dem)
  #tmap::tmap_mode("plot")
  #map
  expect_true("tmap" %in% class(map))
})

test_that("map is a tmap", {
  xs <- sf::st_read(system.file("extdata", "xs.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  dem <- get_dem(xs)
  map <- get_terrain_map(xs, dem)
  tmap::tmap_mode("plot")
  map
  expect_true("tmap" %in% class(map))
})