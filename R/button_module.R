button_ui <- function(id, label) {
  actionButton(NS(id, "btn"), label = label)
}

button_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$btn)
  })
}