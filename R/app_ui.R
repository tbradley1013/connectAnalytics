#' @import shiny
ca_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    shinyjs::useShinyjs(),
    golem_add_external_resources(),
    div(
      id = "loading-content",
      class = "loading-content-screen",
      h2("Loading...", style = "margin-top:300px")
    ),
    shinyjs::hidden(
      div(
        id = "app-content",
        navbarPageWithInputs(
          "connectAnalytics",
          id = "navbar-tabs",
          theme = shinythemes::shinytheme("flatly"),
          tabPanel(
            title = "Content",
            icon = icon("user"),
            mod_04_content_ui("04_content_ui_1"), # This module has no UI - it is simply loading the users content
            mod_05_usage_ui("05_usage_ui_1", admin = FALSE)
          ),
          tabPanel(
            title = "User Info",
            icon = shiny::icon("user"),
            mod_03_userinfo_ui("03_userinfo_ui_1")
          ),
          tabPanel(
            title = "Admin",
            icon = icon("toolbox"),
            mod_05_usage_ui("05_usage_ui_2_admin", admin = TRUE) 
          ), 
          inputs = tagList(
            mod_01_server_connect_ui("01_server_connect_ui_1"),
            mod_02_username_ui("02_username_ui_1")
          )
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
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
