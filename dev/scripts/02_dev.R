# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
#attachment::att_amend_desc()
usethis::use_package("assertthat")
usethis::use_package("bslib")
usethis::use_package("dplyr")
usethis::use_dev_package("fluvgeo", remote = "FluvialGeomorph/fluvgeo@*release")
usethis::use_package("gt")
usethis::use_package("htmltools")
usethis::use_package("leaflet")
usethis::use_package("leaflet.extras")
usethis::use_package("leafpm")
usethis::use_package("mapedit")
usethis::use_package("purrr")
usethis::use_package("sf")
usethis::use_package("shinyWidgets")
usethis::use_package("shinybusy")
usethis::use_package("terra")
usethis::use_package("tibble")
usethis::use_package("tidyterra")

## Add modules ----
## Create a module infrastructure in R/
#golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
#golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
#golem::add_fct("helpers", with_test = TRUE)
#golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
#golem::add_js_file("script")
#golem::add_js_handler("handlers")
#golem::add_css_file("custom")
#golem::add_sass_file("custom")
#golem::add_any_file("file.json")

## Add internal datasets ----
## If you have data in your package
#usethis::use_data_raw(name = "my_dataset", open = FALSE)

# Agentic context
# Scaffold only when initializing a repository. Existing repository-owned
# customizations are intentionally not overwritten.
if (!file.exists("AGENTS.md")) {
  reproducibleai::use_agentic_context(
    path = ".",
    profiles = c("base", "r-package")
  )
}

# AGENTS.md routes Codex to the maintained goals, architecture, decisions, and
# workflows required for each task. A separate bootstrap prompt and transcript
# export are no longer part of the development process.
reproducibleai::validate_agentic_context(path = ".", strict = TRUE)

# Configure Environment Variables
# This app uses the credentials of this app to connect to ESRI web services
# https://usace-mvr.maps.arcgis.com/home/item.html?id=b5e3ddc1fbb444cda8d5837693e45739
# On the service's page, go to the settings page, and scroll to the credentials section. 
# There you will find the Client ID and Client Secret

## Open the `.Renviron` file for your system
usethis::edit_r_environ()

## Add ARCGIS_CLIENTID, ARCGIS_CLIENTSECRET, and ARCGIS_HOST to `.Renviron`.
## Do not place credential values in this script or another committed file.

# Restart R, then verify names are configured without printing secret values.
required_arcgis_vars <- c(
  "ARCGIS_CLIENTID",
  "ARCGIS_CLIENTSECRET",
  "ARCGIS_HOST"
)
stopifnot(all(nzchar(Sys.getenv(required_arcgis_vars))))

# Test authentication interactively when required:
# fluvgeo::arcgis_auth()


## Run application locally
golem::document_and_reload()
run_app()

## Tests ----
## Add one line by test you want to create
#usethis::use_test("app")

# Documentation

## Vignette ----
#usethis::use_vignette("ohwm2")
#devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
#usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
#covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
#usethis::use_github()

# GitHub Actions
#usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
#usethis::use_github_action_check_release()
#usethis::use_github_action_check_standard()
#usethis::use_github_action_check_full()
# Add action for PR
#usethis::use_github_action_pr_commands()

# Circle CI
#usethis::use_circleci()
#usethis::use_circleci_badge()

# Jenkins
#usethis::use_jenkins()

# GitLab CI
#usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/scripts/03_deploy.R
rstudioapi::navigateToFile("dev/scripts/03_deploy.R")
