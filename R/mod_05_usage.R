# Module UI
  
#' @title   mod_05_usage_ui and mod_05_usage_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_05_usage
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_05_usage_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_05_usage
#' @export
#' @keywords internal
    
mod_05_usage_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_05_usage_ui("05_usage_ui_1")
    
## To be copied in the server
# callModule(mod_05_usage_server, "05_usage_ui_1")
 
