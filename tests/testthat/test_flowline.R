test_that("map is a tmap", {
  xs <- sf::st_read(system.file("extdata", "xs.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  dem <- get_dem(xs)
  
})