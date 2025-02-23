test_that("WGS84 input - dem is a terra::SpatRaster", {
  xs <- sf::st_read(system.file("extdata", "xs.geojson", 
                                package = "tieredassessment"), quiet = TRUE)
  dem <- get_dem(xs)
  #terra::plot(dem)
  expect_true("SpatRaster" %in% class(dem))
})

test_that("web mercator input - dem is a terra::SpatRaster", {
  xs <- sf::st_read(system.file("extdata", "xs.geojson", 
                                package = "tieredassessment"), quiet = TRUE)
  xs_3857 <- sf::st_transform(xs, crs = 3857)
  dem <- get_dem(xs_3857)
  #terra::plot(dem)
  expect_true("SpatRaster" %in% class(dem))
})
