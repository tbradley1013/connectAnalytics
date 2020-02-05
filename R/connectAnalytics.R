#' Run the Shiny Application
#'
#' @param host the host server for the RStudio Connect to be connected to. 
#' Defaults to the environment variable "RSTUDIO_CONNECT_SERVER"
#' @param api_key a valid RStudio Connect API key 
#' @param user the desired user to log in as. If NULL (default) it will default
#' to the session$user (recommended)
#' @param switch_user logical; if TRUE a button appears in the navbar that will 
#' allow users to switch which connect user analytics data they are looking at. 
#' This could be useful if your company wants to allow users to see how others are
#' performing. 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
connectAnalytics <- function(..., host = Sys.getenv("RSTUDIO_CONNECT_SERVER"), 
                             api_key = Sys.getenv("RSTUDIO_CONNECT_API_KEY"),
                             user = NULL, switch_user = TRUE) {
  golem::with_golem_options(
    app = shiny::shinyApp(ui = ca_ui, server = ca_server), 
    golem_opts = list(host = host, api_key = api_key, user = user, switch_user = switch_user, ...)
  )
}

