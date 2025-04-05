test_that("write sf to shp", {
  data <- sf::st_read(system.file("extdata", "fl.shp", 
                                    package = "tieredassessment"), quiet = TRUE)
  filename <- "bogus"
  save_test_data(data, filename)
  test_data_folder <- file.path(here::here(), "inst", "extdata")
  expect_true(file.exists(file.path(test_data_folder, 
                                    paste0(filename, ".shp"))))
  # cleanup
  files_to_delete <- dir(test_data_folder ,pattern="bogus*")
  file.remove(file.path(test_data_folder, files_to_delete))
})