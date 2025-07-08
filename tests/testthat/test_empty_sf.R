test_that("returns empty sf object?", {
  sf <- empty_sf()
  expect_true("sf" %in% class(sf))
})