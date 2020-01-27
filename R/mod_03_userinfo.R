# Module UI
  
#' @title   mod_03_userinfo_ui and mod_03_userinfo_server
#' @description  A shiny Module to collect and display user information for a selected
#' RStudio Connect user
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param r a reactiveValues object with a `client` object that is a valid RStudio Connect
#' connection
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
    h5("User Info Table"),
    fluidRow(
      reactable::reactableOutput(ns("user_table"))
    ),
    h5("User Applications"),
    fluidRow(
      reactable::reactableOutput(ns("user_content"))
    )
    
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
    
    all_users <- connectapi::get_users(r$client, page_size = Inf)
    
    r$all_users <- all_users
    
    all_users[all_users$username == r$username, ]
    # connectapi::get_users(r$client, prefix = list(username = r$username))
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
          detail_field("Last Updated", format(user$updated_time, "%b %d, %Y")),
          detail_field("Last Active", format(user$active_time, "%b %d, %Y")),
          detail_field("User GUID", user$guid)
        )
      }
    )
  })
  
  output$user_content <- reactable::renderReactable({
    req(user_info(), r$publisher, r$content) 
    
    dat <- dplyr::filter(r$content, owner_username == r$username)
    
    reactable::reactable(
      dat[, c("id", "name", "title", "url")],
      columns = list(
        id = reactable::colDef("ID"),
        name = reactable::colDef("Name"),
        title = reactable::colDef("Title"),
        url = reactable::colDef(
          "URL", 
          cell = function(value){
            htmltools::tags$a(href = value, target = "_blank", "link")
          }
        )
      ),
      details = function(index){
        
      }
    )
  })
  
  outputOptions(output, "user_table", suspendWhenHidden = FALSE)
  outputOptions(output, "user_content", suspendWhenHidden = FALSE)
  
}
    
## To be copied in the UI
# mod_03_userinfo_ui("03_userinfo_ui_1")
    
## To be copied in the server
# callModule(mod_03_userinfo_server, "03_userinfo_ui_1")
 
