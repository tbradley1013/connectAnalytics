# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module(name = "01_server_connect") # Name of the module
golem::add_module(name = "02_username") # Name of the module
golem::add_module(name = "03_userinfo")
golem::add_module(name = "04_content")

## 2.2 Add dependencies

usethis::use_dev_package("connectapi")
usethis::use_package("purrr")
usethis::use_package("shinyjs")
usethis::use_package("shinythemes")
usethis::use_dev_package("reactable")
usethis::use_package("dplyr")
# usethis::use_package("thinkr") # To call each time you need a new package

## 2.3 Add tests

usethis::use_test("app")

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file("button_click")
# golem::add_js_handler( "handlers" )
golem::add_css_file("styles")

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("connectAnalytics")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
