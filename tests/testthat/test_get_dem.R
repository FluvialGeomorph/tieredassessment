test_that("WGS84 input - dem is a terra::SpatRaster", {
  xs_geojson <- sf::st_read(system.file("extdata", "xs.geojson", 
                                package = "tieredassessment"), quiet = TRUE)
  xs <- sf::st_transform(xs_geojson, crs = 3857)
  dem <- get_dem(xs)
  #terra::plot(dem)
  expect_true("SpatRaster" %in% class(dem))
})

test_that("web mercator input - dem is a terra::SpatRaster", {
  xs_shp <- sf::st_read(system.file("extdata", "xs.geojson", 
                                package = "tieredassessment"), quiet = TRUE)
  xs <- sf::st_transform(xs_shp, crs = 3857)
  dem <- get_dem(xs)
  #terra::plot(dem)
  expect_true("SpatRaster" %in% class(dem))
})

test_that("expect error - WGS84 input", {
  xs <- sf::st_read(system.file("extdata", "xs.geojson", 
                                package = "tieredassessment"), quiet = TRUE)
  expect_error(get_dem(xs))
})

test_that("leaflet from shapefile", {
  xs_mapedit_shp <- sf::st_read(system.file("extdata", "xs_mapedit.shp", 
                                    package = "tieredassessment"), quiet = TRUE)
  #plot(xs_mapedit_shp)
  xs <- sf::st_transform(xs_mapedit_shp, crs = 3857) # Web Mercator
  st_crs(xs_mapedit_shp) == st_crs(xs)
  dem <- get_dem(xs)
  #terra::plot(dem)
  expect_true("SpatRaster" %in% class(dem))
})