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
          plotly::plotlyOutput(ns("admin_line_graph"))
        ),
        fluidRow(
          shinydashboard::tabBox(
            title = "Shiny Usage",
            tabPanel(
              title = "By Date",
              plotly::plotlyOutput(ns("shiny_usage_by_date")) 
            ),
            tabPanel(
              title = "By User",
              plotly::plotlyOutput(ns("shiny_usage_by_user"))
            )
            
          ),
          shinydashboard::tabBox(
            title = "Static Usage",
            tabPanel(
              title = "By Date",
              plotly::plotlyOutput(ns("static_usage_by_date"))
            ),
            tabPanel(
              title = "By User",
              plotly::plotlyOutput(ns("static_usage_by_user"))
            )
          )
          
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
    
    overall_usage_line(r$shiny_usage_all, r$static_usage_all, from = input$content_dates[1], to = input$content_dates[2], username = r$username, admin = TRUE)
  })
  
  usage_shiny <- reactive({
    req(r$shiny_usage_all, r$content, r$all_users)
    
    usage_info_join(r$shiny_usage_all, r$content, r$all_users)
  })
  
  usage_static <- reactive({
    req(r$static_usage_all, r$content, r$all_users)
    
    usage_info_join(r$static_usage_all, r$content, r$all_users)
  })
  
  output$shiny_usage_by_date <- plotly::renderPlotly({
    req(usage_shiny())
    
    usage_by_date(usage_shiny(), time_col = started, type = "Shiny App")
    
  })
  
  
  output$shiny_usage_by_user <- plotly::renderPlotly({
    req(usage_shiny())
    
    usage_by_user(usage_shiny(), type = "Shiny App")
  })
  
  output$static_usage_by_date <- plotly::renderPlotly({
    req(usage_static())
    
    usage_by_date(usage_static(), time_col = time, type = "Static Content")
  })
  
  output$static_usage_by_user <- plotly::renderPlotly({
    req(usage_static())
    
    usage_by_user(usage_static(), type = "Static Content")
  })
}
    
## To be copied in the UI
# mod_06_admin_ui("06_admin_ui_1")
    
## To be copied in the server
# callModule(mod_06_admin_server, "06_admin_ui_1")
 
