#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    shinyjs::useShinyjs(),
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPageWithInputs(
      "connectAnalytics",
      theme = shinythemes::shinytheme("flatly"),
      tabPanel(
        title = "Content",
        icon = icon("user"),
        mod_04_content_ui("04_content_ui_1"),
        mod_05_usage_ui("05_usage_ui_1")
      ),
      tabPanel(
        title = "User Info",
        icon = shiny::icon("user"),
        mod_03_userinfo_ui("03_userinfo_ui_1")
      ),
      inputs = tagList(
        mod_01_server_connect_ui("01_server_connect_ui_1"),
        mod_02_username_ui("02_username_ui_1")
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  # uncomment once package is built
  addResourcePath(
    'www', system.file('app/www', package = 'connectAnalytics')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # shiny::includeScript("button_click.js"),
    # shiny::includeCSS("/www/styles.css")
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css"),
    tags$script(src = "www/button_click.js")
  )
}
