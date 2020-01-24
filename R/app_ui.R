#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    shinyjs::useShinyjs(),
    shinythemes::shinytheme("darkly"),
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPageWithInputs(
      "connectAnalytics",
      inputs = tagList(
        mod_01_server_connect_ui("01_server_connect_ui_1")
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  # addResourcePath(
  #   'www', system.file('app/www', package = 'connectAnalytics')
  # )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
