# Module UI
  
#' @title   mod_04_content_ui and mod_04_content_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_04_content
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_04_content_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}
    
# Module Server
    
#' @rdname mod_04_content
#' @export
#' @keywords internal
    
mod_04_content_server <- function(input, output, session, r){
  ns <- session$ns
  
  observe({
    req(r$client)
    
    r$content <- connectapi::get_content(r$client, limit = Inf)
  })
}
    
## To be copied in the UI
# mod_04_content_ui("04_content_ui_1")
    
## To be copied in the server
# callModule(mod_04_content_server, "04_content_ui_1")
 
