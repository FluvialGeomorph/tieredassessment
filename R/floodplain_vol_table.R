#' @title Floodplain Volume Table
#' @description Create a floodplain volume table. 
#' @param channel_vol     numeric; Channel volume.
#' @param floodplain_vol  numeric; Floodplain volume
#' @returns a gt table
#' @export
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom gt gt tab_stubhead cols_label html fmt_number
floodplain_vol_table <- function(channel_vol, floodplain_vol) {
  assert_that(is.numeric(channel_vol),
              msg = "channel_vol must be numeric")
  assert_that(is.numeric(floodplain_vol),
              msg = "floodplain_vol must be numeric")
  
  vols_tbl <- 
    tibble(
      features = c("Channel", "Floodplain"), 
      volume   = c(channel_vol, floodplain_vol)
    )
  
  vol_table <- 
    gt(vols_tbl,
       rowname_col = "features") |>
    tab_stubhead(label = "Features") |>
    cols_label(volume = html("Volume (m<sup>3</sup>)")) |>
    fmt_number(columns = volume, decimals = 0) |>
    tab_options(
      column_labels.font.weight = "bold",
      table.font.size = "small",
      column_labels.padding = px(2),
      data_row.padding = px(1)
    )
  #vol_table
  return(vol_table)
}
