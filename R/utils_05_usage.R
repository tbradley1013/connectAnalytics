# Utility functions for the Usage module


#' @title Create a interactive line graph showing Connect Usage
#' @description create a interactive plotly graph showing the 
#' daily usage trends for shiny application, static content, and 
#' overall
#' 
#' @param shiny_usage a shiny_usage dataset from the 
#' \link{\code{connectapi::get_usage_shiny}} function
#' @param static_usage a data from the 
#' \link{\code{connectapi::get_usage_static}} function
#' @param from the start date of the interested query
#' @param to the end data of the interested query
#' 
#' @return a plotly object
overall_usage_line <- function(shiny_usage, static_usage, from, to, username, admin = FALSE){
  
  if (admin){
    title <- glue::glue("Overall Content Views for Entire Server ({format(from)} - {format(to)})")
  } else {
    title <- glue::glue("Overall Content Usage for {username}'s Content ({format(from)} - {format(to)})")
  }
  
  usage_tbl <- overall_usage_tbl(shiny_usage, static_usage, from, to)
  
  p <- plotly::plot_ly(
    usage_tbl,
    x = ~date, 
    y = ~value, 
    color = ~name,
    hoverinfo = "text",
    text = ~glue::glue(
      "<b>{name}</b>",
      "<b>Date</b>: {date}", 
      "<b>Count</b>: {value}",
      .sep = "<br>"
    )
  ) %>% 
    plotly::add_lines() %>% 
    plotly::layout(
      title = title,
      yaxis = list(
        title = "Count"
      ), 
      xaxis = list(title = "")
    )
  
  return(p)
}


#' @title Get a tbl showing overall usage by date
#' @description Get a tbl from content usage data that shows the total number
#' of sessions each day for static and shiny applications and overall
#' 
#' @param shiny_usage a shiny_usage dataset from the 
#' \link{\code{connectapi::get_usage_shiny}} function
#' @param static_usage a data from the 
#' \link{\code{connectapi::get_usage_static}} function
#' @param from the start date of the interested query
#' @param to the end data of the interested query
#' 
#' @return a tbl in long format
overall_usage_tbl <- function(shiny_usage, static_usage, from, to){
  shiny_usage <- shiny_usage %>% 
    dplyr::mutate(date = lubridate::date(started)) %>% 
    dplyr::count(date, name = "n_shiny")
  
  static_usage <- static_usage %>% 
    dplyr::mutate(date = lubridate::date(time)) %>% 
    dplyr::count(date, name = "n_static")
  
  date_tbl <- tibble::tibble(date = seq.Date(from = from, to = to, by = 1))
  
  out <- date_tbl %>% 
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
    tidyr::pivot_longer(cols = `# Shiny`:`# Total`, values_drop_na = FALSE)
  
  return(out)
}

usage_info_join <- function(usage, content, users){
  out <- usage %>% 
    dplyr::left_join(
      content[, c("guid", "owner_username", "title")],
      by = c("content_guid" = "guid")
    ) %>% 
    dplyr::left_join(
      users[, c("username", "first_name", "last_name", "guid")],
      by = c("user_guid" = "guid")
    ) %>% 
    dplyr::mutate_at(dplyr::vars(username, first_name, last_name), list(~ifelse(is.na(.), "Anonymous", .))) %>% 
    dplyr::mutate(title = ifelse(is.na(title), "Removed Content", title)) 
  
  return(out)
}

usage_by_date <- function(usage, time_col, type = "Shiny App"){
  time_col <- dplyr::enquo(time_col)
  
  type <- match.arg(type, c("Shiny App", "Static Content"))
  
  usage %>% 
    dplyr::mutate(date = lubridate::date(!!time_col)) %>% 
    dplyr::count(date, title) %>% 
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
        title = "Count"
      ),
      xaxis = list(title = ""),
      title = glue::glue("{type} Usage By Date"),
      barmode = "stack"
    )
}

usage_by_user <- function(usage, type = "Shiny App"){
  type <- match.arg(type, c("Shiny App", "Static Content"))
  
  usage %>% 
    dplyr::count(title, username, first_name, last_name) %>% 
    plotly::plot_ly(
      x = ~title,
      y = ~n,
      color = ~username,
      type = "bar",
      hoverinfo = "text",
      text = ~glue::glue(
        "<b>App Name</b>: {title}",
        "<b>User</b>: {first_name} {ifelse(last_name == 'Anonymous', '', last_name)}", 
        "<b>Count</b>: {n}",
        .sep = "<br>"
      )
    ) %>% 
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = ""),
      yaxis = list(title = "Count"),
      title = glue::glue("{type} Usage By User")
    )
}