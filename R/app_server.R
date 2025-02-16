#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom htmltools tags
#' @importFrom purrr map
#' @importFrom leaflet leaflet addProviderTiles setView
#' @importFrom dplyr %>% bind_rows
#' @importFrom mapedit editMod
#' @importFrom leafpm addPmToolbar pmToolbarOptions
#' @importFrom sf st_as_sf st_sfc
#' @importFrom tmap tm_basemap tm_view renderTmap tmapProxy
#' @importFrom terra plot
#' 
#' @noRd
app_server <- function(input, output, session) {
  
  # Define the cross sections
  xs <- reactive({
    sf <- data.frame(Seq = integer()) %>%
      st_as_sf(geometry = st_sfc(), 
               crs = 4326)
    return(sf)
  })
  
  # Define an empty dem
  dem <- reactive({
    raster <- matrix(1:25, nrow=5, ncol=5) %>%
      terra::rast()
    return(raster)
  })
  
  # Define the draw_xs_map  
  draw_xs_map <- leaflet() %>%
    setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
    addProviderTiles("Esri.WorldTopoMap")
  
  # Define the xs_map
  tmap_mode("view")
  xs_map <- 
    tm_basemap("Esri.WorldTopoMap") + 
    tm_view(use_WebGL = FALSE,
            #set_bounds = TRUE,
            set_view = c(-93.85, 37.45, 4))
  
  # Define the mapedit module
  draw_xs <- callModule(editMod,
                        id = "xs_editor",
                        leafmap = draw_xs_map,
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
  
  # Create the xs map
  output$terrain_map <- renderTmap({
    xs_map
  })
  
  # Events
  observeEvent(input$get_terrain, {
    # get finished xs
    xs <- dplyr::bind_rows(shiny::req(xs()),
                           draw_xs()$finished)
    
    # overwrite dem
    dem <- get_dem(xs)
    
    tmapProxy("terrain_map", session, {
      xs_extent <- fluvgeo::map_extent(xs)
      get_terrain_map(xs, dem) +
      tm_view(set_view = c(mean(c(xs_extent$xmin-xs_extent$xmax)) + xs_extent$xmin,
                           mean(c(xs_extent$ymin-xs_extent$ymax)) + xs_extent$ymin,
                           14))
    })
  })
  
  # Instructions
  ## create draw xs page instructions
  output$draw_xs_instructions <- renderUI({
    steps <- c('Zoom to the desired AOI.', 
               'Draw cross sections.', 
               'Click the "Get Terrain" button below to retrieve the digital elevation model (DEM).',
               'View the terrain using the "View Terrain" item on the top menu.')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
  
  ## create view terrain page instructions
  output$view_terrain_instructions <- renderUI({
    steps <- c('Do this.', 
               'Then do that.', 
               'Then do this other important thing.',
               'Finally all your dreams will come true..')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
  
  ## create calc xs page instructions
  output$calc_xs_instructions <- renderUI({
    steps <- c('Do this.', 
               'Then do that.', 
               'Then do this other important thing.',
               'Finally all your dreams will come true..')
    ul <- htmltools::tags$ul(
      purrr::map(steps, function(.x) tags$li(.x)))
  })
  options = list(launch.browser=TRUE)
} 
