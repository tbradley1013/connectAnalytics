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
  
  )
}
    
# Module Server
    
#' @rdname mod_02_username
#' @export
#' @keywords internal
    
mod_02_username_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_02_username_ui("02_username_ui_1")
    
## To be copied in the server
# callModule(mod_02_username_server, "02_username_ui_1")
 
