#' @title Empty sf object
#' @description Returns an empty sf object.
#' @returns An empty sf object
#' @importFrom sf st_as_sf st_sfc
empty_sf <- function() {
  sf <- data.frame(ReachName = as.character()) %>%
    st_as_sf(geometry = st_sfc(), crs = 3857)  # ensure Web Mercator
  return(sf)
}
