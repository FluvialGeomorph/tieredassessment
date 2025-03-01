test_that("map is a tmap", {
  xs <- sf::st_read(system.file("extdata", "xs.geojson", 
                                package = "tieredassessment"), quiet = TRUE)
  new_xs <- sf::st_transform(xs, crs = 3857) # Web Mercator
  dem <- get_dem(new_xs)
  map <- get_terrain_map(new_xs, dem)
  #tmap::tmap_mode("plot")
  #map
  expect_true("tmap" %in% class(map))
})

test_that("map is a tmap", {
  xs <- sf::st_read(system.file("extdata", "xs.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  new_xs <- sf::st_transform(xs, crs = 3857) # Web Mercator
  # clean_xs <- new_xs %>%
  #   mutate(Seq = as.numeric(Seq)) %>%
  #   mutate(ReachName = " ") %>%
    #fluvgeo::xs_bearing(.)
  
  dem <- get_dem(new_xs)
  map <- get_terrain_map(new_xs, dem)
  #tmap::tmap_mode("plot")
  #map
  expect_true("tmap" %in% class(map))
})