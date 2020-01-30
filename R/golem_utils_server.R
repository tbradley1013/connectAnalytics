# Inverted versions of in, is.null and is.na
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

# Removes the null from a vector
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

# If x is null, return y, otherwise return x
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}
# If x is NA, return y, otherwise return x
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

# typing reactiveValues is too long
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList


# Busy indicator server side
# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}


# Function for styling reactable expanding sections
detail_field <- function(name, ...) {
  if (any(is.na(...))) NULL
  else tagList(div(class = "detail-label", name), ...)
}


overall_usage_line <- function(shiny_usage, static_usage, from, to, username, admin = FALSE){
  
  shiny_usage <- shiny_usage %>% 
    dplyr::mutate(date = lubridate::date(started)) %>% 
    dplyr::count(date, name = "n_shiny")
  
  static_usage <- static_usage %>% 
    dplyr::mutate(date = lubridate::date(time)) %>% 
    dplyr::count(date, name = "n_static")
  
  date_tbl <- tibble::tibble(date = seq.Date(from = from, to = to, by = 1))
  
  if (admin){
    title <- glue::glue("Overall Content Views for Entire Server ({format(from)} - {format(to)})")
  } else {
    title <- glue::glue("Overall Content Usage for {username}'s Content ({format(from)} - {format(to)})")
  }
  
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
    plotly::plot_ly(
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