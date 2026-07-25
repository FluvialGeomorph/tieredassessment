#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @param skin Optional normalized application skin. When omitted, the packaged
#'   defaults and any configured override are loaded.
#' @param skin_file Optional path to a downstream skin override file.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  skin = NULL,
  skin_file = NULL,
  ...
) {
  if (!is.null(skin) && !is.null(skin_file)) {
    stop("Supply only one of `skin` or `skin_file`.", call. = FALSE)
  }

  if (is.null(skin)) {
    skin <- load_app_skin(override_file = skin_file)
  } else {
    skin <- normalize_app_skin(skin)
    validate_app_skin(skin)
  }

  with_golem_options(
    app = shinyApp(
      ui = function(request) {
        app_ui(request, skin = skin)
      },
      server = function(input, output, session) {
        app_server(input, output, session, skin = skin)
      },
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
