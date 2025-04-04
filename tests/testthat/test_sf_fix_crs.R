test_that("sf - fixes 4326 incorrecly set to 3857", {
  # Features saved from mapedit module
  xs_shp <- sf::st_read(system.file("extdata", "xs.shp", 
                                    package = "tieredassessment"), quiet = TRUE)
  obj <- xs_shp
  sf::st_crs(obj)$epsg                  # 3857, but this is incorrect, see bbox
  sf::st_bbox(obj)                      # bbox is latlon dd
  fixed_obj <- sf_fix_crs(obj)
  sf::st_crs(fixed_obj)$epsg            # 4326, this is now correct, see bbox
  sf::st_bbox(fixed_obj)                # bbox is latlon dd
  expect_equal(sf::st_crs(fixed_obj)$epsg, 4326)
  
  # Now let's try a transform from 4326 to 3857
  xs_3857 <- sf::st_transform(fixed_obj, crs = 3857)
  sf::st_crs(xs_3857)$epsg              # 3857, correct, see bbox
  sf::st_bbox(xs_3857)                  # bbox is now meters
  expect_equal(sf::st_crs(xs_3857)$epsg, 3857)
})
test_that("sf - handles 3857 already set correctly", {
  # Features saved from mapedit module
  fl_shp <- sf::st_read(system.file("extdata", "fl.shp", 
                                    package = "tieredassessment"), quiet = TRUE)
  obj <- fl_shp
  sf::st_crs(obj)$epsg                  # 3857, this is correct, see bbox
  sf::st_bbox(obj)                      # bbox is meters
  fixed_obj <- sf_fix_crs(obj)
  sf::st_crs(fixed_obj)$epsg            # 3857, this is still correct, see bbox
  sf::st_bbox(fixed_obj)                # bbox is meters
  expect_equal(sf::st_crs(fixed_obj)$epsg, 3857)
  
  # Now let's try a transform from 3857 to 4326
  fl_4326 <- sf::st_transform(fixed_obj, crs = 4326)
  sf::st_crs(fl_4326)$epsg              # 4326, correct, see bbox
  sf::st_bbox(fl_4326)                  # bbox is now latlon dd
  expect_equal(sf::st_crs(fl_4326)$epsg, 4326)
})
test_that("terra::SpatRast - get_dem", {
  xs_shp <- sf::st_read(system.file("extdata", "xs.shp", 
                                    package = "tieredassessment"), quiet = TRUE)
  sf::st_crs(xs_shp)$epsg              # 3857, see bbox
  sf::st_bbox(xs_shp)                  # bbox is latlon dd
  xs <- sf_fix_crs(xs_shp)     
  sf::st_crs(xs)$epsg                  # 4326, see bbox
  sf::st_bbox(xs)                      # bbox is latlon dd
  xs_3857 <- sf::st_transform(xs, crs = 3857) 
  sf::st_crs(xs_3857)$epsg             # 3857, see bbox
  sf::st_bbox(xs_3857)                 # bbox is meters
  # use xs_3857 to get dem in 3857
  dem <- get_dem(xs_3857)
  sf::st_crs(dem)$epsg                 # 3857, see bbox
  sf::st_bbox(dem)                     # bbox is meters
  expect_equal(sf::st_crs(dem)$epsg, 3857)
})
