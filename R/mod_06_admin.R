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
    shinyjs::hidden(
      div(
        id = ns("admin-tab"),
        fluidRow(
          div(
            dateRangeInput(
              inputId = ns("content_dates"),
              label = "Select Date Range",
              start = (Sys.Date() - lubridate::days(7)),
              end = Sys.Date()
            ),
            style = "margin-left:20px"
          )
        ),
        fluidRow(
          plotly::plotlyOutput("admin_line_graph")
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_06_admin
#' @export
#' @keywords internal
    
mod_06_admin_server <- function(input, output, session, r){
  ns <- session$ns
  
  observe({
    if (!r$admin){
      shiny::hideTab(inputId = "navbar-tabs", target = "Admin")
    }
  })
  
  observe({
    req(r$admin)
    
    shinyjs::show("admin-tab")
  })
  
  output$admin_line_graph <- plotly::renderPlotly({
    req(r$shiny_usage_all, r$static_usage_all, r$admin)
    
    overall_usage_line(r$shiny_usage_all, r$static_usage_all, input = input, r = r, admin = TRUE)
  })
}
    
## To be copied in the UI
# mod_06_admin_ui("06_admin_ui_1")
    
## To be copied in the server
# callModule(mod_06_admin_server, "06_admin_ui_1")
 
