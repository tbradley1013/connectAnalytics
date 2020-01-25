# Module UI
  
#' @title   mod_03_userinfo_ui and mod_03_userinfo_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_03_userinfo
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_03_userinfo_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(paste("User:", textOutput(ns("user_name"))))
  )
}
    
# Module Server
    
#' @rdname mod_03_userinfo
#' @export
#' @keywords internal
    
mod_03_userinfo_server <- function(input, output, session, r){
  ns <- session$ns
  
  output$user_name <- renderText(r$username)
  
  user_info <- reactive({
    req(r$client)
    
    connectapi::get_users(r$client, prefix = list(username = r$username))
  })
  
  observe({
    req(user_info())
    
    if ("administrator" %in% user_info()$user_role){
      r$admin <- TRUE
    } else {
      r$admin <- FALSE
    }
  })
  
  
}
    
## To be copied in the UI
# mod_03_userinfo_ui("03_userinfo_ui_1")
    
## To be copied in the server
# callModule(mod_03_userinfo_server, "03_userinfo_ui_1")
 
