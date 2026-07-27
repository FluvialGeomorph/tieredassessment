test_that("cached volume lookup matches fluvgeo floodplain volume", {
  dem <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 30,
    ymin = 0,
    ymax = 30,
    crs = "EPSG:3857"
  )
  terra::values(dem) <- c(
    101, 102, 104,
    100, 103, 105,
    99, 101, 106
  )
  trend <- terra::rast(dem)
  terra::values(trend) <- c(
    100, 100.5, 101,
    100, 100.5, 101,
    100, 100.5, 101
  )
  lookup <- prepare_floodplain_volume_lookup(dem, trend)

  for (level in c(100, 101.5, 103, 106.25)) {
    water_surface <- trend + (level - 100)
    expected <- fluvgeo::floodplain_volume(dem, water_surface)
    actual <- calculate_floodplain_volume(lookup, level)

    expect_equal(actual, expected, tolerance = 1e-8)
  }
})

test_that("single-region classification matches fluvgeo classification", {
  points <- sf::st_as_sf(
    data.frame(
      id = 1:4,
      x = c(0, 5, 15, 25),
      y = c(0, 0, 0, 0)
    ),
    coords = c("x", "y"),
    crs = 3857
  )
  channel <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(-2, -2, 10, -2, 10, 2, -2, 2, -2, -2),
      ncol = 2,
      byrow = TRUE
    ))), crs = 3857)
  )
  floodplain <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(-2, -2, 20, -2, 20, 2, -2, 2, -2, -2),
      ncol = 2,
      byrow = TRUE
    ))), crs = 3857)
  )
  expected <- fluvgeo::xs_pts_classify(
    points,
    channel,
    floodplain,
    buffer_distance = 2
  )

  channel_only <- points
  channel_only$floodplain <- 7
  channel_only <- update_xs_polygon_classification(
    channel_only,
    channel,
    field = "channel",
    buffer_distance = 2
  )
  expect_equal(channel_only$channel, expected$channel)
  expect_equal(channel_only$floodplain, rep(7, nrow(points)))

  floodplain_only <- points
  floodplain_only$channel <- 9
  floodplain_only <- update_xs_polygon_classification(
    floodplain_only,
    floodplain,
    field = "floodplain",
    buffer_distance = 2
  )
  expect_equal(floodplain_only$floodplain, expected$floodplain)
  expect_equal(floodplain_only$channel, rep(9, nrow(points)))
})

test_that("water-surface polygon cache reuses levels and evicts LRU entries", {
  calls <- 0L
  resolver <- function(rem, water_surface_elevation, flowline) {
    calls <<- calls + 1L
    list(level = water_surface_elevation, call = calls)
  }
  cache <- new_water_surface_polygon_cache(max_entries = 2)

  first <- resolve_cached_water_surface_polygon(
    cache, 101, NULL, NULL, resolver
  )
  second <- resolve_cached_water_surface_polygon(
    first$cache, 102, NULL, NULL, resolver
  )
  first_again <- resolve_cached_water_surface_polygon(
    second$cache, 101, NULL, NULL, resolver
  )
  third <- resolve_cached_water_surface_polygon(
    first_again$cache, 103, NULL, NULL, resolver
  )

  expect_equal(calls, 3L)
  expect_false(first$cache_hit)
  expect_false(second$cache_hit)
  expect_true(first_again$cache_hit)
  expect_false(third$cache_hit)
  expect_setequal(names(third$cache$entries), c(
    "rem_101.000000",
    "rem_103.000000"
  ))
  expect_false("rem_102.000000" %in% names(third$cache$entries))
})

test_that("polygon cache can be shared by Channel and Floodplain levels", {
  calls <- 0L
  resolver <- function(rem, water_surface_elevation, flowline) {
    calls <<- calls + 1L
    list(level = water_surface_elevation)
  }
  cache <- new_water_surface_polygon_cache(max_entries = 4)

  channel <- resolve_cached_water_surface_polygon(
    cache, 105.5, NULL, NULL, resolver
  )
  floodplain <- resolve_cached_water_surface_polygon(
    channel$cache, 105.5, NULL, NULL, resolver
  )

  expect_equal(calls, 1L)
  expect_true(floodplain$cache_hit)
  expect_equal(floodplain$polygon, channel$polygon)
})
