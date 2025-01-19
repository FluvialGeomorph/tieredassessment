test_that("dem is a terra::SpatRaster", {
  xs <- sf::st_read(system.file("extdata", "xs.geojson", 
                              package = "tieredassessment"))
  dem <- get_dem(xs)
  #terra::plot(dem)
  expect_true("SpatRaster" %in% class(dem))
})