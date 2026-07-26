test_that("Draw XS search uses a response-aware formatter", {
  options <- draw_xs_search_options()
  formatter <- as.character(options$formatData)

  expect_identical(options$propertyName, "display_name")
  expect_identical(options$propertyLoc, c("lat", "lon"))
  expect_match(options$url, "format=jsonv2", fixed = TRUE)
  expect_match(formatter, "function(controlOrResults, response)", fixed = TRUE)
  expect_match(formatter, "Array.isArray(response)", fixed = TRUE)
  expect_match(formatter, "result.display_name || result.name", fixed = TRUE)
  expect_match(formatter, "Number.isFinite(lat)", fixed = TRUE)
  expect_match(formatter, "Number.isFinite(lon)", fixed = TRUE)
})

test_that("Draw XS map includes the hardened OSM search control", {
  map <- get_draw_xs_leaflet(zoom = 4)
  methods <- vapply(map$x$calls, `[[`, character(1), "method")
  search_call <- map$x$calls[[which(methods == "addSearchOSM")]]
  options <- search_call$args[[1]]

  expect_s3_class(map, "leaflet")
  expect_length(which(methods == "addSearchOSM"), 1)
  expect_identical(options$propertyName, "display_name")
  expect_identical(options$propertyLoc, c("lat", "lon"))
  expect_match(
    as.character(options$formatData),
    "Array.isArray(response)",
    fixed = TRUE
  )
})
