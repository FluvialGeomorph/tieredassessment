test_that("Does 'auth_user' function for auth work?", {
  unset_arc_token()
  # ensure .Renviron has variables set, restart R session
 
  token <- arcgis_auth()
  expect_true("httr2_token" %in% class(token))
})
