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
      plotly::plotlyOutput(ns("usage_line_graph"))
    ),
    fluidRow(
      plotly::plotlyOutput(ns("shiny_usage_by_date")),
      plotly::plotlyOutput(ns("shiny_usage_by_user"))
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
  
  output$usage_line_graph <- plotly::renderPlotly({
    req(r$shiny_usage, r$static_usage)
    
    shiny_usage <- r$shiny_usage %>% 
      dplyr::mutate(date = lubridate::date(started)) %>% 
      dplyr::count(date, name = "n_shiny")
    
    static_usage <- r$static_usage %>% 
      dplyr::mutate(date = lubridate::date(time)) %>% 
      dplyr::count(date, name = "n_static")
    
    date_tbl <- tibble::tibble(date = seq.Date(from = input$content_dates[1], to = input$content_dates[2], by = 1))
    n_days <- as.numeric(input$content_dates[2] - input$content_dates[1]) - 1
    
    p <- date_tbl %>% 
      dplyr::left_join(
        shiny_usage, 
        by = "date"
      ) %>%  
      dplyr::left_join(
        static_usage, 
        by = "date"
      ) %>% 
      dplyr::mutate_at(dplyr::vars(n_shiny, n_static), list(~ifelse(is.na(.), 0, .))) %>% 
      dplyr::mutate(total = n_shiny + n_static) %>% 
      dplyr::rename(`# Total` = total, `# Shiny` = n_shiny, `# Static` = n_static) %>% 
      tidyr::pivot_longer(cols = `# Shiny`:`# Total`, values_drop_na = FALSE) %>% 
      plotly::plot_ly(x = ~date, y = ~value, color = ~name) %>% 
      plotly::add_lines() %>% 
      plotly::layout(
        title = paste("Overall App Usage in the last", n_days, "days"),
        yaxis = list(
          title = "Count"
        ), 
        xaxis = list(
          title = "Date"
        )
      )
    
    return(p)
  })
  
  
  usage_shiny <- reactive({
    req(r$shiny_usage)
    
    r$shiny_usage %>% 
      dplyr::left_join(
        r$user_content[, c("guid", "owner_username", "title")],
        by = c("content_guid" = "guid")
      ) %>% 
      dplyr::left_join(
        r$all_users[, c("username", "first_name", "last_name", "guid")],
        by = c("user_guid" = "guid")
      ) %>% 
      dplyr::mutate_at(dplyr::vars(username, first_name, last_name), list(~ifelse(is.na(.), "Anonymous", .))) %>% 
      dplyr::mutate(title = ifelse(is.na(title), "Removed Content", title)) 
  })
  
  output$shiny_usage_by_date <- plotly::renderPlotly({
    req(usage_shiny())
    
    usage_shiny() %>% 
      dplyr::mutate(date = lubridate::date(started)) %>% 
      dplyr::count(date, title) %>% 
      # dplyr::mutate(title = ifelse(is.na(title), "Removed Content", title)) %>% 
      plotly::plot_ly(
        x = ~date, 
        y = ~n, 
        color = ~title, 
        type = "bar", 
        hoverinfo = "text",
        text = ~glue::glue(
          "<b>App Name</b>: {title}",
          "<b>Date</b>: {date}", 
          "<b>Count</b>: {n}",
          .sep = "<br>"
        )
      ) %>% 
      plotly::layout(
        yaxis = list(
          title = "App Usage Count"
        ),
        barmode = "stack"
      )
    
  })
  
  
  output$shiny_usage_by_user <- plotly::renderPlotly({
    req(usage_shiny())
    
    usage_shiny() %>% 
      dplyr::count(title, username, first_name, last_name) %>% 
      plotly::plot_ly(
        x = ~title,
        y = ~n,
        color = ~username,
        type = "bar",
        hoverinfo = "text",
        text = ~glue::glue(
          "<b>App Name</b>: {title}",
          "<b>User</b>: {first_name} {last_name}", 
          "<b>Count</b>: {n}",
          .sep = "<br>"
        )
      ) %>% 
      plotly::layout(
        barmode = "stack",
        xaxis = list(title = ""),
        yaxis = list(title = "Count"),
        title = "App usage by user"
      )
  })
}
    
## To be copied in the UI
# mod_05_usage_ui("05_usage_ui_1")
    
## To be copied in the server
# callModule(mod_05_usage_server, "05_usage_ui_1")
 
