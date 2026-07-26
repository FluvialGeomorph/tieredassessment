#' Build the Draw XS map
#'
#' Adds an OpenStreetMap search formatter that is compatible with both the
#' one-argument and two-argument formatter contracts used by Leaflet Search.
#'
#' @param zoom Numeric initial map zoom.
#'
#' @return A leaflet map.
#' @noRd
get_draw_xs_leaflet <- function(zoom = 4) {
  get_leaflet(search = FALSE, zoom = zoom) %>%
    setView(lng = -93.85, lat = 37.45, zoom = zoom) %>%
    addSearchOSM(options = draw_xs_search_options())
}

#' Configure Draw XS location search
#'
#' @return A list of Leaflet Search options.
#' @noRd
draw_xs_search_options <- function() {
  searchOptions(
    url = paste0(
      "https://nominatim.openstreetmap.org/search",
      "?format=jsonv2&limit=10&q={s}"
    ),
    propertyName = "display_name",
    propertyLoc = c("lat", "lon"),
    formatData = draw_xs_search_formatter(),
    collapsed = TRUE,
    autoCollapse = TRUE,
    autoCollapseTime = 20000,
    minLength = 3,
    hideMarkerOnCollapse = TRUE,
    zoom = 14
  )
}

#' Normalize OpenStreetMap search results for Leaflet Search
#'
#' Leaflet Search versions used by leaflet.extras have called `formatData`
#' with either `(response)` or `(control, response)`. Reading the wrong
#' argument makes every result label render as the literal `undefined`.
#'
#' @return A JavaScript formatter.
#' @importFrom htmlwidgets JS
#' @noRd
draw_xs_search_formatter <- function() {
  JS(
    paste(
      "function(controlOrResults, response) {",
      "  var results = Array.isArray(response)",
      "    ? response : controlOrResults;",
      "  var records = {};",
      "  if (!Array.isArray(results)) return records;",
      "  results.forEach(function(result) {",
      "    if (!result) return;",
      "    var label = result.display_name || result.name;",
      "    var lat = Number(result.lat);",
      "    var lon = Number(result.lon);",
      "    if (typeof label !== 'string' || !label.trim()) return;",
      "    if (!Number.isFinite(lat) || !Number.isFinite(lon)) return;",
      "    records[label] = L.latLng(lat, lon);",
      "  });",
      "  return records;",
      "}"
    )
  )
}
