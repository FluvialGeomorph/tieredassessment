xs_plot <- function(xs, fl, fl_pts, dem) {
  plot(dem)
  lines(terra::vect(xs), col = "black")
  lines(terra::vect(fl), col = "blue")
  points(vect(filter(fl_pts, POINT_M == min(POINT_M))), col = "green", cex = 5)
  points(vect(filter(fl_pts, POINT_M == max(POINT_M))), col = "red", cex = 5)
  points(fl_pts, col = "white")
}

test_that("check for valid flowline points", {
  fl_mapedit <- sf::st_read(system.file("extdata", "fl_mapedit.shp", 
                                        package = "tieredassessment"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream" 
  dem <- get_dem(fl_3857)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 100
  flowline_points <- flowline_points(flowline, dem, station_distance)
  xs_mapedit <- sf::st_read(system.file("extdata", "xs_mapedit.shp", 
                                package = "tieredassessment"), quiet = TRUE)
  xs_fix <- sf_fix_crs(xs_mapedit)
  cs_3857 <- sf::st_transform(xs_fix, crs = 3857) # Web Mercator
  xs_lines <- cross_section(xs_3857, flowline_points)
  #xs_plot(xs_lines, flowline, flowline_points, dem)
  expect_true(fluvgeo::check_cross_section(xs_lines))
})
