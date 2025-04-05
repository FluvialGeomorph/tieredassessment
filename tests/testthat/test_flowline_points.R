test_that("map is a tmap", {
  fl <- sf::st_read(system.file("extdata", "fl.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  dem <- get_dem(fl)
  station_distance = 100
  fl_pts <- flowline_points(fl, dem, station_distance)
  expect_true("sf" %in% class(fl_pts))
  expect_true("POINT_X" %in% colnames(fl_pts))
  expect_true("POINT_Y" %in% colnames(fl_pts))
  expect_true("POINT_M" %in% colnames(fl_pts))
  expect_true("Z" %in% colnames(fl_pts))
})