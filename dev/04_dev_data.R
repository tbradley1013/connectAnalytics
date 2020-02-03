

tmp <- connectapi::connect(host = Sys.getenv("RSTUDIO_CONNECT_SERVER"), api_key = Sys.getenv("RSTUDIO_CONNECT_API_KEY"))


if (file.exists("dev/data/dev_shiny_usage.rds")){
  dev_shiny_usage <- readr::read_rds("dev/data/dev_shiny_usage.rds")
} else {
  dev_shiny_usage <- connectapi::get_usage_shiny(tmp, from = (Sys.Date() - lubridate::days(7)), asc_order = FALSE, limit = Inf)
  readr::write_rds(dev_shiny_usage, "dev/data/dev_shiny_usage.rds")
}

if (file.exists("dev/data/dev_static_usage.rds")){
  dev_static_usage <- readr::read_rds("dev/data/dev_static_usage.rds")
} else {
  dev_static_usage <- connectapi::get_usage_static(tmp, from = (Sys.Date() - lubridate::days(7)), asc_order = FALSE, limit = Inf)
  readr::write_rds(dev_static_usage, "dev/data/dev_static_usage.rds")
}

if (file.exists("dev/data/dev_content.rds")){
  dev_content <- readr::read_rds("dev/data/dev_content.rds")
} else {
  dev_content <- connectapi::get_content(tmp, limit = Inf)
  readr::write_rds(dev_content, "dev/data/dev_content.rds")
}

if (file.exists("dev/data/dev_users.rds")){
  dev_users <- readr::read_rds("dev/data/dev_users.rds")
} else {
  dev_users <- connectapi::get_users(tmp, page_size = Inf)
  readr::write_rds(dev_users, "dev/data/dev_users.rds")
}

shiny_usage_join <- usage_info_join(dev_shiny_usage, dev_content, dev_users)
static_usage_join <- usage_info_join(dev_static_usage, dev_content, dev_users)

dev_from <- Sys.Date() - lubridate::days(7)
dev_to <- Sys.Date()

shiny_usage_join %>% 
  usage_by_date_tbl(time_col = "started", from = dev_from, to = dev_to) 

usage_continuous <- shiny_usage_join %>% 
  tidyr::pivot_longer(cols = c(started, ended), names_to = "name", values_to = "datetime", values_drop_na = TRUE) %>% 
  dplyr::arrange(datetime) %>% 
  dplyr::mutate(user_count = ifelse(name == "started", 1, -1), 
                user_count = cumsum(user_count)) 


p <- usage_continuous %>% 
  ggplot2::ggplot(ggplot2::aes(datetime, user_count)) + 
  ggplot2::geom_step() + 
  ggplot2::theme_bw() 

shiny_usage_join %>% 
  dplyr::mutate(app_time = difftime(ended, started, units = "mins")) %>%
  dplyr::mutate(
    title = factor(title, levels = unique(title)),
    title = forcats::fct_reorder(title, app_time, .fun = mean),
    app_time = round(as.numeric(app_time), 2)
  ) %>% 
  {ggplot2::ggplot(., ggplot2::aes(x = title, y = app_time)) + 
  ggplot2::geom_boxplot(outlier.shape = NA) +
  # ggplot2::geom_jitter() + 
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = "",
    y = "Appliation Run Time (minutes)",
    title = "Distribution of Application Run Time"
  ) +
  ggplot2::theme_bw() } %>% 
  plotly::ggplotly()

plotly::ggplotly(p)

usage_continuous %>% 
  plotly::plot_ly(
    x = ~datetime,
    y = ~user_count,
    type = "scatter",
    mode = "lines",
    line = list(shape = "hv")
  )


tmp_time <- tibble::tibble(
  datetime = seq.POSIXt(from = min(usage_continuous$datetime), to = max(usage_continuous$datetime), by = "min")
)

tmp_time %>% 
  dplyr::full_join(
    usage_continuous, by = "datetime"
  ) %>% 
  dplyr::arrange(datetime) %>% 
  tidyr::fill(user_count, .direction = "down") %>% 
  plotly::plot_ly(
  # usage_continuous, 
  x = ~datetime,
  y = ~user_count,
  type = "scatter",
  mode = "lines"
  # line = list(shape = "hvh")
)


tmp_time %>% 
  dplyr::full_join(
    usage_continuous, by = "datetime"
  ) %>% 
  dplyr::arrange(datetime) %>% 
  tidyr::fill(user_count, .direction = "down")  %>% 
  ggplot2::ggplot(ggplot2::aes(datetime, user_count)) + 
  ggplot2::geom_step() + 
  ggplot2::theme_bw()



shiny_usage_join %>% 
  dplyr::mutate(content = title,
                id = paste0("shiny-", id),
                title = glue::glue("{title}", "User: {username}", "Started: {format(started, '%b %d, %Y %H:%M:%S')}", "Ended: {format(ended, '%b %d, %Y %H:%M:%S')}", .sep = "\r\n\r\n")) %>% 
  dplyr::select(start = started, end = ended, content, title) %>% 
  dplyr::bind_rows({
    static_usage_join %>% 
      dplyr::mutate(content = title,
                    id = paste0("static-", id),
                    title = glue::glue("{title}", "User: {username}", "Time: {format(time, '%b %d, %Y %H:%M:%S')}", .sep = "\n")) %>% 
      dplyr::select(start = time, content, title)
  }) %>% 
  timevis::timevis(
    options = list(
      tooltip = list(
        delay = 100
        # template = htmlwidgets::JS("function(item, element, data) {
        #    var my_html = data.content + '<br>Started: ' + data.start + '<br>Ended: ' + data.end;
        #    return my_html;
        # }")
      )
    )
  )
  