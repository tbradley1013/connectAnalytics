# Module UI
  
#' @title   mod_06_admin_ui and mod_06_admin_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_06_admin
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_06_admin_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_06_admin
#' @export
#' @keywords internal
    
mod_06_admin_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_06_admin_ui("06_admin_ui_1")
    
## To be copied in the server
# callModule(mod_06_admin_server, "06_admin_ui_1")
 
