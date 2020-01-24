# Module UI
  
#' @title   mod_01_server_connect_ui and mod_01_server_connect_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_01_server_connect
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_01_server_connect_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}
    
# Module Server
    
#' @rdname mod_01_server_connect
#' @export
#' @keywords internal
    
mod_01_server_connect_server <- function(input, output, session, r){
  ns <- session$ns
  
  observe({
    if (is.null(r$connect_server) | is.null(r$api_key)){
      
    }
  })
}
    
## To be copied in the UI
# mod_01_server_connect_ui("01_server_connect_ui_1")
    
## To be copied in the server
# callModule(mod_01_server_connect_server, "01_server_connect_ui_1")
 
