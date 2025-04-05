#' @title Save Test Data
#' @description Function for saving spatial objects created within a Shiny app 
#'              to support package testing. 
#' @param data      sf or terra SpatRaster object; data to be saved in the 
#'                  package for testing
#' @param filename  filename 
#'
#' @returns nothing, saves data to test folder
#' 
save_test_data <- function(data, filename) {
  assert_that("sf" %in% class(data) | "SpatRaster" %in% class(data), 
              msg = "must be an sf or SpatRaster object")

  test_data_folder <- file.path(here::here(), "inst", "extdata")
  
  if("sf" %in% class(data)) {
    sf::st_write(data, file.path(test_data_folder, 
                                 paste0(filename, ".shp")),
                 delete_dsn = TRUE)
  }
  if("SpatRaster" %in% class(data)) {
    terra::writeRaster(data, file.path(test_data_folder, 
                                       paste0(filename, ".tif")),
                       overwrite = TRUE)
  }
}