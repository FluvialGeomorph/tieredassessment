#' @title Create a Cross Section Discharge Table
#'
#' @description
#' Creates a cross section discharge table for the channel portion of the
#' specified cross section.
#'
#' @export
#' @param xs_pts        sf; A cross section lines feature class.
#' @param xs_number     integer; The cross section `Seq` number of the
#'                      requested cross section.
#' @param bf_estimate   numeric; Detrended bankfull estimate (units:
#'                      detrended feet).
#' @param mannings_n    numeric; The Manning's n coeficient.
#'
#' @return a `gt` object
#'
#' @importFrom fluvgeo slope_sinuosity xs_geometry
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom nhdplusTools discover_nhdplus_id subset_nhdplus
#' @importFrom gt gt fmt_number cols_label_with cols_label tab_options px
#'
xs_discharge_table <- function(xs_pts, xs_number, bf_estimate, mannings_n) {

  # Calculate the slope from adjacent cross sections
  xs_ss <- xs_pts %>%
    group_by(.data$Seq) %>%
    slice_min(.data$DEM_Z, n = 1, with_ties = FALSE) %>%
    rename(Z = DEM_Z) %>%
    slope_sinuosity(lead_n = 1, lag_n = 1, use_smoothing = FALSE, 
                    vert_units = "ft") %>%
    ungroup()
  
  # Get reach slope from nhdPlus flowline
  xs <- xs_ss %>%
    filter(.data$Seq == xs_number)
  
  print(paste0("Seq: ", xs$Seq))
  print(paste0("X: ", round(xs$POINT_X), 
               " Y: ", round(xs$POINT_Y)))
  
  point_sfc <- sf::st_sfc(sf::st_point(x = c(xs$POINT_X, xs$POINT_Y), 
                                       dim = "XY"), 
                          crs = 3857)
  
  start_comid <- discover_nhdplus_id(
    point = point_sfc, 
    nldi_feature = "comid",
    raindrop = TRUE)
  print(paste0("comid: ", start_comid$comid[1]))
  
  output_file <- tempfile(fileext = ".gpkg")
  on.exit(unlink(output_file), add = TRUE)
  nhd_flowline <- subset_nhdplus(
    comids = c(start_comid$comid[1]),
    output_file = output_file,
    nhdplus_data = "download",
    overwrite = TRUE, status = FALSE, flowline_only = TRUE)
  print(paste0("GNIS Name: ", nhd_flowline$NHDFlowline_Network$gnis_name))
  
  nhd_slope <- nhd_flowline[1]$NHDFlowline_Network$slope

  dims_table_long <- prepare_xs_discharge_values(
    xs_pts = xs_pts,
    xs_number = xs_number,
    bf_estimate = bf_estimate,
    mannings_n = mannings_n,
    nhd_slope = nhd_slope
  )
  
  gt_table <- dims_table_long |>
    gt() |>
    cols_label_with(fn = tools::toTitleCase) |>
    cols_label(label = "Variable") |>
    fmt_number(columns = value, decimals = 1) |>
    fmt_number(columns = value, rows = label == "Slope (S)", decimals = 4) |>
    tab_options(
      column_labels.font.weight = "bold",
      table.font.size = "small",
      column_labels.padding = px(2),
      data_row.padding = px(1),
      table.margin.left = px(1),
      table.margin.right = px(1))
  #gt_table
  return(gt_table)
}

#' Prepare DEM-derived discharge values
#'
#' @param xs_pts Cross-section points.
#' @param xs_number Cross-section sequence number.
#' @param bf_estimate Relative bankfull elevation.
#' @param mannings_n Manning's roughness coefficient.
#' @param nhd_slope Reach slope.
#'
#' @return A long-form data frame of discharge values.
#' @noRd
prepare_xs_discharge_values <- function(
  xs_pts,
  xs_number,
  bf_estimate,
  mannings_n,
  nhd_slope
) {
  xs_pts_channel <- xs_pts %>%
    filter(.data$Seq == xs_number) %>%
    filter(.data$channel == 1)

  dims <- fluvgeo::xs_geometry(
    xs_points = xs_pts_channel,
    detrend_elevation = bf_estimate
  )
  drainage_area <- unique(xs_pts_channel$Watershed_Area_SqMile)
  drainage_area <- if (length(drainage_area) > 0L) {
    as.numeric(drainage_area[[1]])
  } else {
    NA_real_
  }
  nhd_slope <- as.numeric(nhd_slope[[1]])
  channel_flow <- (1.486 / mannings_n) *
    dims$xs_area *
    (dims$xs_depth^(2 / 3)) *
    (nhd_slope^(1 / 2))

  dims_table <- tibble(
    xs_area = dims$xs_area,
    xs_width = dims$xs_width,
    xs_depth = dims$xs_depth,
    drainage_area = drainage_area,
    R_proxy = dims$xs_depth,
    S_proxy = nhd_slope,
    Q = channel_flow,
    V = channel_flow / dims$xs_area
  )

  dims_table %>%
    pivot_longer(everything()) %>%
    filter(!is.na(.data$value)) %>%
    mutate(
      units = recode(
        .data$name,
        xs_area = "sq ft",
        xs_width = "ft",
        xs_depth = "ft",
        drainage_area = "sq mi",
        R_proxy = "ft",
        S_proxy = "",
        Q = "cfs",
        V = "ft sec"
      ),
      label = recode(
        .data$name,
        xs_area = "XS Area (A)",
        xs_width = "XS Width",
        xs_depth = "XS Mean Depth",
        drainage_area = "Drainage Area",
        R_proxy = "XS Hydraulic Radius (R)",
        S_proxy = "Slope (S)",
        Q = "Channel Flow (Q)",
        V = "Channel Velocity (V)"
      )
    ) %>%
    relocate("label", .before = "name") %>%
    select(-"name")
}
