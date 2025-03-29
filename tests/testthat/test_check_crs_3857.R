test_that("input is not 3857", {
  xs_geojson <- sf::st_read(system.file("extdata", "xs.geojson", 
                                package = "tieredassessment"), quiet = TRUE)
  expect_error(check_crs_3857(xs_geojson))
})

test_that("input is 3857", {
  xs_geojson <- sf::st_read(system.file("extdata", "xs.geojson", 
                                        package = "tieredassessment"), quiet = TRUE)
  xs <- sf::st_transform(xs_geojson, crs = "EPSG:3857") # Web Mercator
  expect_true(check_crs_3857(xs))
})
