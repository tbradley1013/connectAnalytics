# Module UI
  
#' @title   mod_02_username_ui and mod_02_username_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_02_username
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_02_username_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      uiOutput(ns("username_ui")),
      actionButton(
        inputId = ns("change_username"),
        label = "Change User",
        class = "btn-success",
        style = "margin: 5px auto;",
        width = "100px"
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_02_username
#' @export
#' @keywords internal
    
mod_02_username_server <- function(input, output, session, r){
  ns <- session$ns
  
  observe({
    if (golem::get_golem_options("switch_user")){
      shinyjs::show("change_username")
    }
  })
  
  # ask for username input if user is null
  dialog <- reactive({
    dialog <- modalDialog(
      textInput(
        inputId = ns("username_mod"),
        label = "Username",
        value = (r$username %||% "")
      ),
      title = "Change User",
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          inputId = ns("submit"),
          label = "Change!",
          class = "btn-primary"
        )
      )
    )
  })
  
  observe({
    req(r$client)
    
    if (is.null(r$username)){
      showModal(dialog())
    }
  })
  
  observe({
    req(!is.null(input$username_mod))
    
    if (input$username == ""){
      shinyjs::disable("submit")
    } else {
      shinyjs::enable("submit")
    }
  })
  
  observeEvent(input$submit, {
    r$username <- input$username_mod
    
    removeModal()
  })
  
  
  # Allowing the user to change the username if the app is set to allow it
  output$username_ui <- renderUI({
    textInput(
      inputId = ns("username"),
      label = "Username",
      value = (r$username %||% "")
    )
  })
  
  
  
  observe({
    req(r$username, !is.null(input$username))
    if (input$username == "" | input$username == r$username){
      shinyjs::disable("change_username")
    } else {
      shinyjs::enable("change_username")
    }
  })
  
  observeEvent(input$change_username, {
    req(r$username)
    r$username <- input$username
  })
  
  
}
    
## To be copied in the UI
# mod_02_username_ui("02_username_ui_1")
    
## To be copied in the server
# callModule(mod_02_username_server, "02_username_ui_1")
 
