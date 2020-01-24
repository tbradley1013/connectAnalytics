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
    actionButton(
      inputId = ns("change_username"),
      label = "Change User",
      class = "btn-success",
      style = "position:relative;float:right;margin: 0 5px;"
    )
  )
}
    
# Module Server
    
#' @rdname mod_02_username
#' @export
#' @keywords internal
    
mod_02_username_server <- function(input, output, session, r){
  ns <- session$ns
  
  dialog <- reactive({
    dialog <- modalDialog(
      textInput(
        inputId = ns("username"),
        label = "Username"
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
  
  observeEvent(input$change_username, {
    showModal(dialog())
  })
  
  observe({
    req(!is.null(input$username))
    
    if (input$username == ""){
      shinyjs::disable("submit")
    } else {
      shinyjs::enable("submit")
    }
  })
  
  observeEvent(input$submit, {
    r$username <- input$username
    
    removeModal()
  })
}
    
## To be copied in the UI
# mod_02_username_ui("02_username_ui_1")
    
## To be copied in the server
# callModule(mod_02_username_server, "02_username_ui_1")
 
