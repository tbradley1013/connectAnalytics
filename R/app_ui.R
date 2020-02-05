#' @import shiny
ca_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    # shinyjs::useShinyjs(),
    golem_add_external_resources(),
    div(
      id = "loading-content",
      class = "loading-content-screen",
      h2("Loading...", style = "margin-top:300px")
    ),
    shinyjs::hidden(
      div(
        id = "app-content",
        shinydashboard::dashboardPage(
          title = (golem::get_golem_options("window_title") %||% golem::get_golem_options("title")),
          # skin = "black",
          shinydashboard::dashboardHeader(
            title = golem::get_golem_options("title")
          ),
          shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                "Content",
                tabName = "content",
                icon = icon("dashboard")
              ),
              shinydashboard::menuItem(
                "User",
                tabName = "users",
                icon = icon("users")
              ),
              shinydashboard::menuItem(
                "Admin",
                tabName = "admin",
                icon = icon("users-cog")
              ),
              dateRangeInput(
                inputId = "content_dates",
                label = "Select Date Range",
                start = (Sys.Date() - lubridate::days(7)),
                end = Sys.Date()
              ),
              mod_02_username_ui("02_username_ui_1"),
              mod_01_server_connect_ui("01_server_connect_ui_1")
            )
          ),
          shinydashboard::dashboardBody(
            shinyjs::useShinyjs(),
            shinydashboard::tabItems(
              shinydashboard::tabItem(
                tabName = "content",
                mod_04_content_ui("04_content_ui_1"), # This module has no UI - it is simply loading the users content
                mod_05_usage_ui("05_usage_ui_1", admin = FALSE)
              ),
              shinydashboard::tabItem(
                tabName = "users",
                mod_03_userinfo_ui("03_userinfo_ui_1")
              ),
              shinydashboard::tabItem(
                tabName = "admin",
                mod_05_usage_ui("05_usage_ui_2_admin", admin = TRUE)
              )
            )
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
  
  fav <- golem::get_golem_options("favicon")
  
  if (!is.null(fav)){
    addResourcePath(
      "fav", dirname(normalizePath(fav))
    )
    
    fav_ico <- file.path("fav", basename(fav))
  } else fav_ico <- "www/favicon.ico"
  # fav_ico <- ifelse(is.null(fav), "www/favicon.ico", normalizePath(fav))
 
  tags$head(
    golem::activate_js(),
    # shiny::HTML(glue::glue('<link rel="icon", type="image/x-icon" href="{fav_ico}" />')),
    tags$link(rel = "icon", type = "image/x-icon", href = fav_ico),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css"),
    tags$script(src = "www/button_click.js")
    # tags$script(HTML("$('main-header').addClass('fixed');"))
  )
}
