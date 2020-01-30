# Module UI
  
#' @title   mod_05_usage_ui and mod_05_usage_server 
#' @description  A shiny Module to get the usage for the user defined window 
#' to show them how much their content is being used, when it is being used and 
#' who is using it. 
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param r a reactiveValues object
#'
#' @rdname mod_05_usage
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_05_usage_ui <- function(id){
  ns <- NS(id)
  tagList(
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
      plotly::plotlyOutput(ns("usage_line_graph"))
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
    # fluidRow(
    #   ,
    #   
    # )
  )
}
    
# Module Server
    
#' @rdname mod_05_usage
#' @export
#' @keywords internal
    
mod_05_usage_server <- function(input, output, session, r){
  ns <- session$ns
  
  observe({
    req(r$client, r$user_content)
    # browser()
    
    r$shiny_usage_all <- connectapi::get_usage_shiny(
      r$client, 
      # content_guid = r$user_content$guid,
      from = input$content_dates[1], 
      to = (input$content_dates[2] + lubridate::days(1)),
      limit = Inf
    )
    
    r$static_usage_all <- connectapi::get_usage_static(
      r$client, 
      # content_guid = r$user_content$guid,
      from = input$content_dates[1],
      to = (input$content_dates[2] + lubridate::days(1)),
      limit = Inf
    )
    
    r$shiny_usage <- r$shiny_usage_all %>% 
      dplyr::filter(content_guid %in% r$user_content$guid)
    
    r$static_usage <- r$static_usage_all %>% 
      dplyr::filter(content_guid %in% r$user_content$guid)
      
  })
  
  output$usage_line_graph <- plotly::renderPlotly({
    req(r$shiny_usage, r$static_usage)
    
    # This is defined in R/golem_utils_server.R
    overall_usage_line(r$shiny_usage, r$static_usage, from = input$content_dates[1], to = input$content_dates[2], username = r$username, admin = FALSE)
  })
  
  
  usage_shiny <- reactive({
    req(r$shiny_usage, r$user_content, r$all_users)
    
    usage_info_join(r$shiny_usage, r$user_content, r$all_users)
  })
  
  usage_static <- reactive({
    req(r$static_usage)
    
    usage_info_join(r$static_usage, r$user_content, r$all_users)
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
# mod_05_usage_ui("05_usage_ui_1")
    
## To be copied in the server
# callModule(mod_05_usage_server, "05_usage_ui_1")
 
