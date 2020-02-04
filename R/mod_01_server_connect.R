# Module UI
  
#' @title   mod_01_server_connect_ui and mod_01_server_connect_server
#' @description  A shiny Module handling the connection to a RStudio Connect
#' server if the expected environment variables are not valid
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param r a reactiveValues object
#'
#' @rdname mod_01_server_connect
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_01_server_connect_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("change_connection"),
      label = "Change Connection",
      class = "btn-danger",
      style = "position:absolute;bottom:50px;left:0",
      width = "200px"
    )
  )
}
    
# Module Server
    
#' @rdname mod_01_server_connect
#' @export
#' @keywords internal
mod_01_server_connect_server <- function(input, output, session, r){
  ns <- session$ns
  
  dialog <- reactive({
    # define the modal dialog
    dialog <- modalDialog(
      shinyjs::hidden(
        div(
          id = ns("login-failed"),
          helpText(
            "The connection to the specified RStudio Connect server failed",
            style = "color:red;"
          )
        )
      ),
      textInput(
        inputId = ns("connect_server"),
        label = "RStudio Connect Server",
        value = (r$connect_server %||% "")
      ),
      passwordInput(
        inputId = ns("api_key"),
        label = "RStudio Connect API Key",
        value = (r$connect_server %||% "")
      ),
      title = "Enter Connection Info",
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          ns("connect"),
          "Connect!",
          class = "btn-primary"
        )
      )
    )
    
    return(dialog)
  })
  
  
  
  # a safe conneciton function
  connect_safe <- purrr::possibly(connectapi::connect, otherwise = NA)
  
  # if the environmental variables exist than try to connect. If they do not
  # then show the modal popup
  observe({
    if (is.null(r$connect_server) | is.null(r$api_key)){
      showModal(dialog())
    } else {
      r$client <- connect_safe(host = r$connect_server, api_key = r$api_key)
    }
  })
  
  # disable the submit button if the inputs are not entered
  observe({
    req(!is.null(input$connect_server), !is.null(input$api_key))
    
    if (input$connect_server == "" | input$api_key == ""){
      shinyjs::disable(id = "connect")
    } else {
      shinyjs::enable(id = "connect")
    }
  })
  
  # try to connect to the server with the input information
  observeEvent(input$connect, {
    shinyjs::disable("connect")
    r$client <- connect_safe(host = input$connect_server, api_key = input$api_key)  
    shinyjs::enable("connect")
    if (!is.na(r$client)){
      r$connect_server <- input$connect_server
      r$api_key <- input$api_key
      removeModal()
    } else {
      showModal(dialog())
      shinyjs::show("login-failed")
    }
  })
  
  # show dialog if the user choses to change connection
  observeEvent(input$change_connection, {
    showModal(dialog())
  })
}
    
## To be copied in the UI
# mod_01_server_connect_ui("01_server_connect_ui_1")
    
## To be copied in the server
# callModule(mod_01_server_connect_server, "01_server_connect_ui_1")
 
