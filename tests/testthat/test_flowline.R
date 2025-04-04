test_that("map is a tmap", {
  fl_shp <- sf::st_read(system.file("extdata", "fl.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_shp)
  flowline <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  dem <- get_dem(flowline)
  fl <- flowline(flowline, dem)
  expect_true("sf" %in% class(fl))
})
