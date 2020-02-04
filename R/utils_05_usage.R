# Utility functions for the Usage module


#' @title Create a interactive line graph showing Connect Usage
#' @description create a interactive plotly graph showing the 
#' daily usage trends for shiny application, static content, and 
#' overall
#' 
#' @param shiny_usage a shiny_usage dataset from the 
#' \code{connectapi::get_usage_shiny} function
#' @param static_usage a data from the 
#' \code{connectapi::get_usage_static} function
#' @param from the start date of the interested query
#' @param to the end data of the interested query
#' 
#' @return a plotly object
overall_usage_line <- function(usage_tbl, from, to, username, admin = FALSE){
  
  if (admin){
    title <- glue::glue("Overall Content Views for Entire Server ({format(from)} - {format(to)})")
  } else {
    title <- glue::glue("Overall Content Usage for {username}'s Content ({format(from)} - {format(to)})")
  }
  
  # usage_tbl <- overall_usage_tbl(shiny_usage, static_usage, from, to)
  
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
#' \code{connectapi::get_usage_shiny} function
#' @param static_usage a data from the 
#' \code{connectapi::get_usage_static} function
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

#' @title Join usage info with content and user info
#' @description Join the three datasets (usage, content, and users) to return
#' a single dataset that contains infomation about each of the apps that are 
#' used on the Connect system
#' 
#' @param usage a usage data frame (shiny or static) with a content_guid and 
#' user_guid column
#' @param content a content dataframe that has information about each of the 
#' pieces of content on the server. Must have columns: guid, owner_username, 
#' and title
#' @param users a users data frame that has information about the users on the 
#' server. Must have columns: username, first_name, last_name, and guid
#' 
#' @return a single dataset that contains the information about each usage instance
#' including what content it is, who owns the content and who used it
#' @details 
#' If the content GUID does in the usage dataset does not match any entries in the 
#' content dataset than it will be marked as "Removed Content"
#' 
#' If the user GUID does not have a match or if it is NA to begin with than the 
#' user of the content will be marked as "Anonymous"
#' 
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
    dplyr::mutate(
      title = ifelse(is.na(title), "Removed Content", title),
      id = dplyr::row_number()
    ) 
  
  return(out)
}

#' @title Count usage by date and content
#' @description Get a count of all of the usage each day for each piece of
#' content
#' 
#' @param usage a usage tbl output from \code{usage_info_join}
#' @param time_col the quoted name of the time column that you wish to use 
#' to calculate the "date" used. For shiny usage either `started` or `ended` 
#' could be used and for static content usage `time` should be used
#' @param type Either "Shiny App" or "Static Content". This string will determine
#' the title of the plot
#' 
#' @rdname usage_plots
#' 
#' @details 
#' The `usage_by_date_tbl` and `usage_by_user_tbl` functions take a usage 
#' dataframe output from \code{usage_info_join} and create counts of the 
#' usage amounts by either date or user, respectively. 
#' 
#' The `usage_by_date` and `usage_by_user` functions use the above functions
#' to create interactive visualizations for the count data
usage_by_date_tbl <- function(usage, time_col, from, to){
  dates <- seq.Date(from, to, by = 1)
  usage$date <- lubridate::date(usage[[time_col]])
  usage$date <- factor(as.character(usage$date), levels = as.character(dates))
  usage$title <- factor(usage$title, levels = unique(usage$title))
  out <- dplyr::count(usage, date, title, .drop = FALSE)
  
  out$date <- as.Date(as.character(out$date))
  return(out)
}


#' @rdname usage_plots
usage_by_date <- function(usage, time_col, from, to, type = "Shiny App"){
  
  type <- match.arg(type, c("Shiny App", "Static Content"))
  
  usage_count <- usage_by_date_tbl(usage, time_col, from, to)
  
  plotly::plot_ly(
    usage_count,
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
    # width = 1
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


#' @rdname usage_plots
usage_by_user_tbl <- function(usage){
  dplyr::count(usage, title, username, first_name, last_name) 
}

#' @rdname usage_plots
usage_by_user <- function(usage, type = "Shiny App"){
  type <- match.arg(type, c("Shiny App", "Static Content"))
  
  usage_count <- usage_by_user_tbl(usage)
  
  usage_count$username <- forcats::fct_rev(forcats::fct_reorder(usage_count$username, usage_count$n, .fun = sum))
  
  plotly::plot_ly(
    usage_count,
    x = ~username,
    y = ~n,
    color = ~title,
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


usage_by_content <- function(usage, type = "Shiny App"){
  type <- match.arg(type, c("Shiny App", "Static Content"))
  
  usage_count <- usage_by_user_tbl(usage)
  
  usage_count$title <- forcats::fct_rev(forcats::fct_reorder(usage_count$title, usage_count$n, .fun = sum))
  
  plotly::plot_ly(
    usage_count,
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
      title = glue::glue("{type} Usage By Content")
    )
}


#' Count by owner
#' 
#' @param usage a usage dataset
usage_by_owner_tbl <- function(usage){
  dplyr::count(usage, owner_username)
}

usage_by_owner <- function(usage, type = "Shiny App"){
  type <- match.arg(type, c("Shiny App", "Static Content"))
  
  usage_count <- usage_by_owner_tbl(usage)
  
  usage_count$owner_username <- forcats::fct_rev(forcats::fct_reorder(usage_count$owner_username, usage_count$n))
  
  plotly::plot_ly(
    usage_count,
    x = ~owner_username,
    y = ~n,
    type = "bar",
    hoverinfo = "text",
    text = ~glue::glue(
      "<b>App Owner</b>: {owner_username}",
      "<b>Count</b>: {n}",
      .sep = "<br>"
    )
  ) %>% 
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = ""),
      yaxis = list(title = "Count"),
      title = glue::glue("{type} Usage By Content")
    )
}
