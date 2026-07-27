make_test_lines <- function(coordinates, crs = 4326) {
  sf::st_sf(
    geometry = sf::st_sfc(
      lapply(coordinates, sf::st_linestring),
      crs = crs
    )
  )
}

test_that("cross-section snapshots use only the current editor geometry", {
  first_pass <- make_test_lines(list(
    matrix(c(-93.01, 37.00, -93.00, 37.01), ncol = 2, byrow = TRUE)
  ))
  second_pass <- make_test_lines(list(
    matrix(c(-93.02, 37.00, -93.01, 37.01), ncol = 2, byrow = TRUE),
    matrix(c(-93.01, 37.00, -93.00, 37.01), ncol = 2, byrow = TRUE),
    matrix(c(-93.00, 37.00, -92.99, 37.01), ncol = 2, byrow = TRUE)
  ))

  first_snapshot <- prepare_cross_section_geometry_snapshot(first_pass)
  second_snapshot <- prepare_cross_section_geometry_snapshot(second_pass)

  expect_equal(nrow(first_snapshot), 1)
  expect_equal(nrow(second_snapshot), 3)
  expect_equal(second_snapshot$Seq, 1:3)
  expect_false(
    identical(
      sf::st_geometry(first_snapshot),
      sf::st_geometry(second_snapshot)
    )
  )
})

test_that("cross-section snapshots resequence after deletion", {
  geometry <- make_test_lines(list(
    matrix(c(-93.02, 37.00, -93.01, 37.01), ncol = 2, byrow = TRUE),
    matrix(c(-93.00, 37.00, -92.99, 37.01), ncol = 2, byrow = TRUE)
  ))
  geometry$layerId <- c(4, 9)

  snapshot <- prepare_cross_section_geometry_snapshot(geometry)

  expect_equal(snapshot$Seq, 1:2)
  expect_false("layerId" %in% names(snapshot))
})

test_that("flowline snapshot uses current edited geometry", {
  geometry <- make_test_lines(list(
    matrix(c(-93.01, 37.00, -93.00, 37.01), ncol = 2, byrow = TRUE),
    matrix(c(-93.02, 37.00, -92.99, 37.02), ncol = 2, byrow = TRUE)
  ))

  snapshot <- prepare_flowline_geometry_snapshot(geometry)

  expect_equal(nrow(snapshot), 1)
  expect_equal(
    unname(sf::st_bbox(sf::st_transform(snapshot, 4326))[["xmin"]]),
    -93.02,
    tolerance = 1e-6
  )
})

test_that("DEM preflight accepts a small-site request", {
  geometry <- make_test_lines(
    list(matrix(c(0, 0, 500, 500), ncol = 2, byrow = TRUE)),
    crs = 3857
  )

  result <- prepare_dem_request_preflight(
    geometry,
    max_span_m = 10000
  )

  expect_true(result$ok)
  expect_equal(result$request_span_m, 750)
})

test_that("DEM preflight rejects an oversized request before service use", {
  geometry <- make_test_lines(
    list(matrix(c(0, 0, 8000, 100), ncol = 2, byrow = TRUE)),
    crs = 3857
  )

  result <- prepare_dem_request_preflight(
    geometry,
    max_span_m = 10000
  )

  expect_false(result$ok)
  expect_equal(result$reason, "too_large")
  expect_match(result$message, "12.0 km across", fixed = TRUE)
  expect_match(result$message, "allows up to 10.0 km", fixed = TRUE)
})

test_that("DEM preflight rejects geometry outside the supported map", {
  geometry <- make_test_lines(list(
    matrix(c(10, 88, 11, 89), ncol = 2, byrow = TRUE)
  ))

  result <- prepare_dem_request_preflight(geometry)

  expect_false(result$ok)
  expect_equal(result$reason, "out_of_bounds")
})

test_that("DEM service errors distinguish coverage and availability", {
  coverage <- classify_dem_request_error(
    simpleError("Requested extent contains no data")
  )
  unavailable <- classify_dem_request_error(
    simpleError("Failed to connect to server")
  )

  expect_equal(coverage$reason, "no_coverage")
  expect_match(coverage$message, "not available for the drawn site")
  expect_equal(unavailable$reason, "service_unavailable")
  expect_match(unavailable$message, "cross sections remain available")
})

test_that("DEM validation requires finite elevations", {
  finite_dem <- terra::rast(nrows = 2, ncols = 2)
  terra::values(finite_dem) <- c(NA_real_, 100, 101, 102)
  empty_dem <- terra::rast(nrows = 2, ncols = 2)
  terra::values(empty_dem) <- NA_real_

  expect_true(dem_has_finite_elevations(finite_dem))
  expect_false(dem_has_finite_elevations(empty_dem))
})
