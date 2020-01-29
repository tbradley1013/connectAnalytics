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


overall_usage_line <- function(shiny_usage, static_usage, input, r, admin = FALSE){
  
  shiny_usage <- shiny_usage %>% 
    dplyr::mutate(date = lubridate::date(started)) %>% 
    dplyr::count(date, name = "n_shiny")
  
  static_usage <- static_usage %>% 
    dplyr::mutate(date = lubridate::date(time)) %>% 
    dplyr::count(date, name = "n_static")
  
  date_tbl <- tibble::tibble(date = seq.Date(from = input$content_dates[1], to = input$content_dates[2], by = 1))
  
  if (admin){
    title <- glue::glue("Overall Content Views for Entire Server ({format(input$content_dates[1])} - {format(input$content_dates[2])})")
  } else {
    title <- glue::glue("Overall Content Usage for {r$username}'s Content ({format(input$content_dates[1])} - {format(input$content_dates[2])})")
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