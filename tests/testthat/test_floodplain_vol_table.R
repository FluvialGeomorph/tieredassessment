test_that("valid table?", {
  channel_vol <- 3000
  floodplain_vol <- 10000
  fpv_tbl <- floodplain_vol_table(channel_vol, floodplain_vol)
  #fpv_tbl
  expect_true("gt_tbl" %in% class(fpv_tbl))
})