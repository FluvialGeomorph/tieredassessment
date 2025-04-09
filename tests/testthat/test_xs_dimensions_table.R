test_that("check for valid flowline points", {
  fl_mapedit <- sf::st_read(system.file("extdata", "fl_mapedit.shp", 
                                        package = "tieredassessment"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream" 
  dem <- get_dem(fl_3857)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(flowline, dem, station_distance)
  xs_mapedit <- sf::st_read(system.file("extdata", "xs_mapedit.shp", 
                                        package = "tieredassessment"), quiet = TRUE)
  xs_fix <- sf_fix_crs(xs_mapedit)
  xs <- sf::st_transform(xs_fix, crs = 3857) # Web Mercator
  cross_section <- cross_section(xs, flowline_points)
  detrend <- dem              # bogus move until I get detrend function working
  station_distance = 5
  xs_pts <- cross_section_points(cross_section, dem, detrend, station_distance)
  xs_pts <- xs_pts %>%
    dplyr::mutate(POINT_M_units = "m") %>%
    dplyr::mutate(channel = 1) %>%
    dplyr::mutate(dem_units = "ft")
  expect_true(fluvgeo::check_cross_section_points(xs_pts, "station_points"))
  
  xs_number   <- 1
  bf_estimate <- 920
  regions     <- c("USA", "Eastern United States")
  
  t1 <- xs_dimensions_table(xs_pts = xs_pts,
                            xs_number = xs_number,
                            bf_estimate = bf_estimate,
                            regions = regions)
  expect_true(is.data.frame(t1))
})