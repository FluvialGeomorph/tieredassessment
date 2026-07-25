test_that("check for discharge table", {
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  xs_fix <- sf_fix_crs(xs_mapedit)
  xs <- sf::st_transform(xs_fix, crs = 3857) # Web Mercator
  fl_mapedit <- sf::st_read(system.file("extdata", "shiny", "fl_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  fl_fix <- sf_fix_crs(fl_mapedit)
  fl_3857 <- sf::st_transform(fl_fix, crs = 3857) # Web Mercator
  reach_name <- "current stream"
  dem <- get_dem(xs)
  flowline <- flowline(fl_3857, reach_name, dem)
  station_distance = 5
  flowline_points <- flowline_points(flowline, dem, station_distance)
  buffer_distance <- 300
  detrend <- detrend(dem, flowline, flowline_points, buffer_distance)
  rem <- detrend$rem
  trend <- detrend$trend
  cross_section <- cross_section(xs, flowline_points, watershed = "skip")
  station_distance = 5
  xs_pts <- cross_section_points(cross_section, dem, rem, station_distance)
  channel_wse <- 103
  channel_poly <- water_surface_poly(rem, channel_wse, flowline)
  floodplain_wse <- 112
  floodplain_poly <- water_surface_poly(rem, floodplain_wse, flowline)
  buffer_distance <- 5
  xs_pts <- xs_pts_classify(xs_pts, channel_poly, floodplain_poly,
                            buffer_distance)
  
  xs_number   <- 1
  bf_estimate <- channel_wse
  mannings_n <- 0.3
  
  t1 <- xs_discharge_table(
    xs_pts = xs_pts,
    xs_number = xs_number,
    bf_estimate = bf_estimate,
    mannings_n = mannings_n
  )
  t1
  expect_true("gt_tbl" %in% class(t1))
})

test_that("DEM-derived discharge values tolerate missing drainage area", {
  xs_pts <- fluvgeo::sin_riffle_channel_points_sf
  xs_pts$channel <- 1
  xs_pts$Watershed_Area_SqMile <- NA_real_

  values <- prepare_xs_discharge_values(
    xs_pts = xs_pts,
    xs_number = 4,
    bf_estimate = 103.5,
    mannings_n = 0.035,
    nhd_slope = 0.002
  )

  expect_false("Drainage Area" %in% values$label)
  expect_true(all(c(
    "XS Area (A)",
    "XS Width",
    "XS Mean Depth",
    "Channel Flow (Q)"
  ) %in% values$label))
  expect_true(all(is.finite(values$value)))
})
