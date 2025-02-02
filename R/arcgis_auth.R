#' Authenticate to an ArcGIS Enterprise Portal
#'
#' @description Authenticates to an ArcGIS Enterprise Portal.
#' 
#' @return an httr token
#' 
#' @importFrom httr2 oauth_token
#' @importFrom arcgisutils auth_user set_arc_token unset_arc_token arc_token
#'
arcgis_auth <- function() {
  # create a token using a named user account
  user_token <- auth_user(
    username = Sys.getenv("ARCGIS_USER"),
    password = Sys.getenv("ARCGIS_PASSWORD"),
    host = Sys.getenv("ARCGIS_HOST"),
    expiration = 60)
  
  # set the token for the current session
  set_arc_token(token = user_token)
  
  return(arc_token())
}