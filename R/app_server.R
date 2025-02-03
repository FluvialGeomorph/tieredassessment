#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom leaflet leaflet addTiles setView
#' @importFrom dplyr %>% bind_rows
#' @importFrom mapedit editMod
#' @importFrom leafpm addPmToolbar pmToolbarOptions
#' @importFrom sf st_as_sf st_sfc
#' @importFrom terra plot
#' 
#' @noRd
app_server <- function(input, output, session) {
  
  xs <- reactive({
    sf <- data.frame(Seq = integer()) %>%
      st_as_sf(geometry = st_sfc(), 
               crs = 4326)
    return(sf)
  })
  
  # Create the map  
  map <- leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  
  draw_xs <- callModule(editMod,
                        id = "xs_editor",
                        leafmap = map,
                        targetLayerId = xs,
                        editor = "leafpm",
                        editorOptions = list(
                          toolbarOptions = pmToolbarOptions(
                                                 drawMarker = FALSE,
                                                 drawPolygon = FALSE,
                                                 drawCircle = FALSE,
                                                 drawRectangle = FALSE,
                                                 cutPolygon = FALSE,
                                                 position = "topright")
                          ))
  
  ns <- shiny::NS("xs_editor")
  
  observeEvent(input$calc_xs, {
    xs <- bind_rows(shiny::req(xs()),
                    draw_xs()$finished)
    # XS summary
    output$xs_stats <- renderTable({summary(xs)})
    
    #dem <- get_dem(xs)
    
    # output$plot <- renderPlot({
    #   terra::plot(dem)
    #})
  })
}
