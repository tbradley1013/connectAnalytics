#' @import shiny
ca_server <- function(input, output,session) {
  session$onSessionEnded(stopApp)
  
  r <- shiny::reactiveValues(
    connect_server = golem::get_golem_options("host"),
    api_key = golem::get_golem_options("api_key"),
    username = (golem::get_golem_options("user") %||% session$user)
    # from = input$content_dates[1],
    # to = input$content_dates[2]
  )
  
  observe({
    r$from <- input$content_dates[1]
    r$to <- input$content_dates[2]
  })
  # List the first level callModules here
  callModule(mod_01_server_connect_server, "01_server_connect_ui_1", session = session, r = r)
  
  callModule(mod_02_username_server, "02_username_ui_1", session = session, r = r)
  
  callModule(mod_03_userinfo_server, "03_userinfo_ui_1", session = session, r = r)
  
  callModule(mod_04_content_server, "04_content_ui_1", session = session, r = r)
  
  callModule(mod_05_usage_server, "05_usage_ui_1", session = session, r = r, admin = FALSE)
  
  callModule(mod_05_usage_server, "05_usage_ui_2_admin", session = session, r = r, admin = TRUE)
  
  observe({
    # req(r$shiny_usage, r$static_usage)
    
    shinyjs::show("app-content")
    shinyjs::hide("loading-content")
  })
}
