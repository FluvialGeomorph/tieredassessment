#' @title Log a message
#' @description  Logs a system message condition suitable for display in a 
#' Shiny console window.
#' @param msg character; The text of the message to display in the log.
#' @returns message condition
#' @export
#' @importFrom utils capture.output
#' 
log_message <- function(msg) {
  if (is.character(msg)) {                      # simple characters
    message(paste0(
      format(Sys.time(), usetz = TRUE),
      " - ",
      msg)
    )
  } else {                                      # complex objects with output
    message(paste0(
      format(Sys.time(), usetz = TRUE),
      " - ",
      capture.output(msg, type = "output"),
      collapse = "<br>"
    ))
  }
}