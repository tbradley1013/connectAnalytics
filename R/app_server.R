#' @import shiny
app_server <- function(input, output,session) {
  session$onSessionEnded(stopApp)
  
  r <- shiny::reactiveValues(
    connect_server = Sys.getenv("RSTUDIO_CONNECT_SERVER"),
    api_key = Sys.getenv("RSTUDIO_CONNECT_API_KEY")
  )
  # List the first level callModules here
}
