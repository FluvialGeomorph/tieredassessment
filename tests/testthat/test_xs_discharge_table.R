test_that("discharge table uses a supplied cached slope", {
  xs_pts <- fluvgeo::sin_riffle_channel_points_sf
  xs_pts$channel <- 1

  table <- xs_discharge_table(
    xs_pts = xs_pts,
    xs_number = 4,
    bf_estimate = 103.5,
    mannings_n = 0.035,
    reach_slope_result = new_reach_slope_result(
      value = 0.002,
      source = "usgs_nhdplus",
      status = "available",
      reason = NULL,
      attempts = 1,
      message = "USGS NHDPlus reach slope is available."
    )
  )

  expect_s3_class(table, "gt_tbl")
  expect_match(
    paste(unlist(table[["_source_notes"]]), collapse = " "),
    "Slope source: USGS Reach",
    fixed = TRUE
  )
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

test_that("reach slope succeeds without retry when USGS is available", {
  xs_pts <- fluvgeo::sin_riffle_channel_points_sf
  sleeps <- numeric()

  result <- resolve_reach_slope(
    xs_pts = xs_pts,
    xs_number = 4,
    lookup_fun = function(point) 0.002,
    sleep_fun = function(seconds) sleeps <<- c(sleeps, seconds)
  )

  expect_equal(result$status, "available")
  expect_equal(result$source, "usgs_nhdplus")
  expect_equal(result$value, 0.002)
  expect_equal(result$attempts, 1L)
  expect_length(sleeps, 0L)
})

test_that("transient USGS failures use bounded backoff then recover", {
  xs_pts <- fluvgeo::sin_riffle_channel_points_sf
  attempts <- 0L
  sleeps <- numeric()

  result <- resolve_reach_slope(
    xs_pts = xs_pts,
    xs_number = 4,
    lookup_fun = function(point) {
      attempts <<- attempts + 1L
      if (attempts < 3L) {
        stop("temporary service failure")
      }
      0.003
    },
    sleep_fun = function(seconds) sleeps <<- c(sleeps, seconds),
    retry_delays = c(0.25, 0.75)
  )

  expect_equal(result$status, "available")
  expect_equal(result$value, 0.003)
  expect_equal(result$attempts, 3L)
  expect_equal(sleeps, c(0.25, 0.75))
})

test_that("missing USGS responses are retried before DEM fallback", {
  xs_pts <- fluvgeo::sin_riffle_channel_points_sf
  attempts <- 0L
  sleeps <- numeric()

  result <- resolve_reach_slope(
    xs_pts = xs_pts,
    xs_number = 4,
    lookup_fun = function(point) {
      attempts <<- attempts + 1L
      NA_real_
    },
    sleep_fun = function(seconds) sleeps <<- c(sleeps, seconds)
  )

  expect_equal(result$status, "fallback")
  expect_equal(result$source, "dem_local")
  expect_equal(result$reason, "no_coverage_or_unavailable")
  expect_equal(attempts, 3L)
  expect_equal(sleeps, c(0.5, 1.5))
  expect_true(is_usable_reach_slope(result$value))
})

test_that("USGS outage exhausts retries and falls back to DEM slope", {
  xs_pts <- fluvgeo::sin_riffle_channel_points_sf
  attempts <- 0L

  result <- resolve_reach_slope(
    xs_pts = xs_pts,
    xs_number = 4,
    lookup_fun = function(point) {
      attempts <<- attempts + 1L
      stop("service unreachable")
    },
    sleep_fun = function(seconds) NULL
  )

  expect_equal(result$status, "fallback")
  expect_equal(result$source, "dem_local")
  expect_equal(result$reason, "service_unavailable")
  expect_equal(attempts, 3L)
  expect_equal(result$attempts, 3L)
  expect_true(is_usable_reach_slope(result$value))
})

test_that("Local DEM selection uses the selected signed slope only", {
  xs_pts <- fluvgeo::sin_riffle_channel_points_sf

  positive <- resolve_dem_reach_slope(xs_pts, xs_number = 4)
  negative <- resolve_dem_reach_slope(xs_pts, xs_number = 8)

  expect_equal(positive$status, "available")
  expect_equal(positive$source, "dem_local")
  expect_true(positive$value > 0)

  expect_equal(negative$status, "unavailable")
  expect_equal(negative$source, "dem_local")
  expect_equal(negative$reason, "nonpositive_local_dem")
  expect_true(negative$value < 0)
})

test_that("USGS fallback does not replace a negative local slope", {
  xs_pts <- fluvgeo::sin_riffle_channel_points_sf

  result <- resolve_reach_slope(
    xs_pts = xs_pts,
    xs_number = 8,
    lookup_fun = function(point) stop("service unreachable"),
    sleep_fun = function(seconds) NULL
  )

  expect_equal(result$status, "unavailable")
  expect_equal(result$source, "dem_local")
  expect_true(result$value < 0)
  expect_false(is_usable_reach_slope(result$value))
})

test_that("discharge degrades to an explanatory table without any valid slope", {
  unavailable <- new_reach_slope_result(
    value = NA_real_,
    source = NA_character_,
    status = "unavailable",
    reason = "service_unavailable",
    attempts = 3,
    message = "Discharge is temporarily unavailable; other results remain."
  )

  table <- xs_discharge_table(
    xs_pts = fluvgeo::sin_riffle_channel_points_sf,
    xs_number = 4,
    bf_estimate = 103.5,
    mannings_n = 0.035,
    reach_slope_result = unavailable
  )

  expect_s3_class(table, "gt_tbl")
  expect_match(as.character(table[["_data"]][["Details"]][[1]]), "other results")
})
