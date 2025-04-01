#' @title Flowline
#' 
#' @description Takes a newly drawn flowline and uses the dem to ensure
#' the flowline is digitized in the upstream direction.  
#' @param flowline 
#' @param dem 
#'
#' @returns a valid flowline
#' @export
#' 
#' @importFrom fluvgeo sf_line_end_point
#' 
flowline <- function(flowline, dem) {
  # Determine downstream end of flowline
  og_start_pt <- flowline %>%
    sf_line_end_point("start")
    
  og_start_pt_z <- sf_get_z(og_start_pt, dem)
  
  
  og_end_pt   <- sf_line_end_point(flowline, "end")
  
  # Set flowline direction
  
  # Create route
  
  return(fl)
}