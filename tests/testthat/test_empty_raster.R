test_that("returns empty terra SpatRaster object?", {
  raster <- empty_raster()
  expect_true("SpatRaster" %in% class(raster))
})