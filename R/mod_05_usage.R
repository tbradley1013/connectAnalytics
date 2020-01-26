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
    fluidRow(
      div(
        dateRangeInput(
          inputId = ns("content_dates"),
          label = "Select Date Range",
          start = (Sys.Date() - lubridate::days(7)),
          end = (Sys.Date() + lubridate::days(1))
        ),
        style = "width = 300px;margin: 0 auto;"
      )
    ),
    fluidRow(
      textOutput("temp_text"),
      plotly::plotlyOutput(ns("usage_line_graph"))
    )
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
    
    r$shiny_usage <- connectapi::get_usage_shiny(
      r$client, 
      content_guid = r$user_content$guid,
      from = input$content_dates[1], 
      to = input$content_dates[2],
      limit = Inf
    )
    
    r$static_usage <- connectapi::get_usage_static(
      r$client, 
      content_guid = r$user_content$guid,
      from = input$content_dates[1],
      to = input$content_dates[2],
      limit = Inf
    )
  })
  
  output$temp_text <- renderText({
    req(r$shiny_usage, r$static_content)
    
    return("Usage is loaded")
  })
  
  output$usage_line_graph <- plotly::renderPlotly({
    req(r$shiny_usage, r$static_usage)
    
    shiny_usage <- r$shiny_usage %>% 
      dplyr::mutate(date = lubridate::date(started)) %>% 
      dplyr::count(date, name = "n_shiny")
    
    static_usage <- r$static_usage %>% 
      dplyr::mutate(date = lubridate::date(time)) %>% 
      dplyr::count(date, name = "n_static")
    
    p <- shiny_usage %>% 
      dplyr::left_join(
        static_usage, 
        by = "date"
      ) %>% 
      dplyr::mutate(total = sum(c(n_shiny, n_static), na.rm = TRUE)) %>% 
      tidyr::pivot_longer(cols = n_shiny:total, values_drop_na = FALSE) %>% 
      plotly::plot_ly(x = ~date, y = ~value, color = ~name) %>% 
      plotly::add_lines()
    
    return(p)
  })
  
}
    
## To be copied in the UI
# mod_05_usage_ui("05_usage_ui_1")
    
## To be copied in the server
# callModule(mod_05_usage_server, "05_usage_ui_1")
 
