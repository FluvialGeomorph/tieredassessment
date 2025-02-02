test_that("Does testing OAuth token auth work?", {
  unset_arc_token()
  # ESRI Living Atlas
  url_image_server <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  # Get updated key before test
  # https://usace-mvr.maps.arcgis.com/home/item.html?id=b5e3ddc1fbb444cda8d5837693e45739
  test_key <- "3NKHt6i2urmWtqOuugvr9VG1T8lJ7v_6OLXPvNN5QXp0kSS9pjbYWXrIRUGJHAzK7WcE_JgvIC2powyKVWHUVfraxfDqlHZFys5oSXaRZn8Brx5kxeWzBmBjW2g14ePH"
  test_token <- httr2::oauth_token(
                     test_key,
                     arcgis_host = "https://usace-mvr.maps.arcgis.com/")
  set_arc_token(test_token)
  token <- arc_token()
  expect_true("httr2_token" %in% class(token))
  imgsrv  <- arc_open(url_image_server)
  expect_true("ImageServer" %in% class(imgsrv))
})

test_that("Does 'auth_code' function for auth work?", {
  unset_arc_token()
  # ESRI Living Atlas
  url_image_server <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  # enter credentials in browser, copy code, close browser, paste in console
  token <- arcgisutils::auth_code()
  expect_true("httr2_token" %in% class(token))
  set_arc_token(token)
  imgsrv  <- arc_open(url_image_server)
  expect_true("ImageServer" %in% class(imgsrv))
})

test_that("Does 'auth_user' function for auth work?", {
  unset_arc_token()
  # ESRI Living Atlas
  url_image_server <- "https://elevation.arcgis.com/arcgis/rest/services/WorldElevation/Terrain/ImageServer"
  # ensure .Renviron has variables set, restart R session
  user_token <- arcgisutils::auth_user(
    username = Sys.getenv("ARCGIS_USER"),
    password = Sys.getenv("ARCGIS_PASSWORD"),
    host = Sys.getenv("ARCGIS_HOST"),
    expiration = 60)
  set_arc_token(token = user_token)
  token <- arc_token()
  expect_true("httr2_token" %in% class(token))
  imgsrv  <- arc_open(url_image_server)
  expect_true("ImageServer" %in% class(imgsrv))
})

test_that("Does 'auth_binding' function for auth work?", {
  unset_arc_token()
  # arcgisbinding::arc.portal_connect(
  #  url = "https://usace-mvr.maps.arcgis.com/",
  #  user = Sys.getenv("ARCGIS_USER"),
  #  password = Sys.getenv("ARCGIS_PASSWORD"))
  token <- arcgisutils::auth_binding()
  set_arc_token(token)
})

test_that("Does 'auth_client' function for auth work?", {
  perm_token <- arcgisutils::auth_client(
  client = Sys.getenv("ARCGIS_CLIENT"),
  secret = Sys.getenv("ARCGIS_SECRET"),
  host = Sys.getenv("ARCGIS_HOST"),
  expiration = 120)
set_arc_token(perm_token)
})

thest_that("Does 'auth_key' function for auth work?", {
  key_token <- arcgisutils::auth_key(
    api_key = Sys.getenv("ARCGIS_API_KEY"),
    host    = Sys.getenv("ARCGIS_HOST"))
  set_arc_token(key_token)
})
