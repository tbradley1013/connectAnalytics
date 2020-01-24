#' @import shiny
app_server <- function(input, output,session) {
  session$onSessionEnded(stopApp)
  
  r <- shiny::reactiveValues(
    connect_server = Sys.getenv("RSTUDIO_CONNECT_SERVER"),
    api_key = Sys.getenv("RSTUDIO_CONNECT_API_KEY"),
    username = "tbradley"
    # username = session$user
  )
  # List the first level callModules here
  callModule(mod_01_server_connect_server, "01_server_connect_ui_1", session = session, r = r)
  
  callModule(mod_02_username_server, "02_username_ui_1", session = session, r = r)
}
