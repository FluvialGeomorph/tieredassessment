test_that("verify text input", {
  msg_txt_1 <- "This is the message."
  msg_html_1 <- capture.output(log_message(msg_txt_1), type = "message")
  msg_html_1
  expect_message(log_message(msg_txt_1))
})

test_that("verify function output", {
  xs_mapedit <- sf::st_read(system.file("extdata", "shiny", "xs_mapedit.shp",
                                        package = "fluvgeodata"), quiet = TRUE)
  msg_html_2 <- capture.output(log_message(xs_mapedit), type = "message")
  #msg_html_2
  expect_message(log_message(xs_mapedit))
})
