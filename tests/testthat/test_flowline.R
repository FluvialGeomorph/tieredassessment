test_that("map is a tmap", {
  xs_shp <- sf::st_read(system.file("extdata", "xs.shp", 
                                    package = "tieredassessment"), quiet = TRUE)
  xs <- sf::st_transform(xs_shp, crs = 3857) # Web Mercator
  fl_shp <- sf::st_read(system.file("extdata", "fl.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  flowline <- sf::st_transform(fl_shp, crs = 3857) # Web Mercator
  dem <- get_dem(xs)
  
})