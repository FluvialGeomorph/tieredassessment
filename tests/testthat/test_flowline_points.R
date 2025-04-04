test_that("map is a tmap", {
  fl_shp <- sf::st_read(system.file("extdata", "fl.shp", 
                                    package = "tieredassessment"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_shp)
  flowline <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  dem <- get_dem(flowline)
  fl <- flowline(flowline, dem)
  station_distance = 100
  fl_pts <- flowline_points(fl, dem, 100)
  expect_true("sf" %in% class(fl_pts))
  expect_true("x" %in% colnames(fl_pts))
  expect_true("y" %in% colnames(fl_pts))
  expect_true("m" %in% colnames(fl_pts))
  expect_true("z" %in% colnames(fl_pts))
})