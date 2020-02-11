

tmp <- connectapi::connect(host = Sys.getenv("CONNECT_SERVER"), api_key = Sys.getenv("CONNECT_API_KEY"))


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
