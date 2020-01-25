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
    h3(textOutput(ns("user_name"))),
    reactable::reactableOutput("user_table")
  )
}
    
# Module Server
    
#' @rdname mod_03_userinfo
#' @export
#' @keywords internal
    
mod_03_userinfo_server <- function(input, output, session, r){
  ns <- session$ns
  
  output$user_name <- renderText(paste("User:", r$username))
  
  user_info <- reactive({
    req(r$client)
    
    connectapi::get_users(r$client, prefix = list(username = r$username))
  })
  
  observe({
    req(user_info())
    
    if ("administrator" %in% user_info()$user_role){
      r$admin <- TRUE
      r$publisher <- TRUE
    } else {
      if ("publisher" %in% user_info()$user_role){
        r$publisher <- TRUE
      } else {
        r$publisher <- FALSE
      }
      r$admin <- FALSE
    }
    
    
    
  })
  
  output$user_table <- reactable::renderReactable({
    req(user_info())
    
    reactable::reactable(
      user_info()[, c("username", "first_name", "last_name", "user_role")],
      columns = list(
        username = reactable::colDef("Username"),
        first_name = reactable::colDef("First Name"),
        last_name = reactable::colDef("Last Name"),
        user_role = reactable::colDef("User Role")
      ),
      details = function(index){
        user <- user_info()[index,]
        
        div(
          class = "table-detail",
          detail_field("User Email", user$email),
          detail_field("Date Created", format(user$created_time, "%b %d, %Y")),
          detail_field("Last Updated", format(user$upadted_time, "%b %d, %Y")),
          detail_field("Last Active", format(user$active_time, "%b %d, %Y")),
          detail_field("User GUID", user$guid)
        )
      }
    )
  })
  
  output$app_table <- reactable::renderReactable({
    req(user_info(), r$publisher)
    
    
  })
  
}
    
## To be copied in the UI
# mod_03_userinfo_ui("03_userinfo_ui_1")
    
## To be copied in the server
# callModule(mod_03_userinfo_server, "03_userinfo_ui_1")
 
