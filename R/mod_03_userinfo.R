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
      class = "users-table",
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
      dat[, c("id", "name", "title", "url", "created_time", "last_deployed_time")],
      columns = list(
        id = reactable::colDef("ID"),
        name = reactable::colDef("Name"),
        title = reactable::colDef("Title"),
        created_time = reactable::colDef("Time Created", cell = function(value){format(value, "%b %d, %Y %H:%M")}),
        last_deployed_time = reactable::colDef("Last Deployed", cell = function(value){format(value, "%b %d, %Y %H:%M")}),
        url = reactable::colDef(
          "URL", 
          cell = function(value){
            htmltools::tags$a(href = value, target = "_blank", "link")
          }
        )
      ),
      class = "users-table",
      details = function(index){
        usr_cont <- dat[index, ]
        
        div(
          class = "table-detail",
          div(
            class = "detail-header",
            htmltools::tags$a(href = usr_cont$url, target = "_blank", usr_cont$title)
          ),
          div(class = "detail-description", usr_cont$description),
          detail_field("Content GUID", usr_cont$guid),
          detail_field("Access Type", usr_cont$access_type),
          detail_field("Connection Timeout", usr_cont$connection_timeout),
          detail_field("Read Timeout", usr_cont$read_timeout),
          detail_feild("Initial Timeout", usr_cont$init_timeout),
          detail_field("Idle Timeout", usr_cont$idle_timeout),
          detail_field("Max Processes", usr_cont$max_processes),
          detail_field("Min Processes", usr_cont$min_processes),
          detail_field("Max Connections per Process", usr_cont$max_conns_per_process),
          detail_field("Load Factor", usr_cont$load_factor),
          detail_field("Content Category", usr_cont$content_category),
          detail_field("R Version", usr_cont$r_version),
          detail_field("Python Version", usr_cont$py_version),
          detail_field("App Role", usr_cont$app_role),
          detail_field("Is Scheduled?", usr_cont$is_scheduled),
          detail_field("Has Parameters?", usr_cont$has_parameters)
        )
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
 
