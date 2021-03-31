
# remotes::install_github("tbradley1013/connectAnalytics")
library(connectAnalytics)
library(shiny)

# This is here so that RStudio recognizes this script as a shiny app! Leave 
# commented out.
# shinyApp()

user <- Sys.getenv("CONNECT_ANALYTICS_USER")
if (user == "") user <- NULL

connectAnalytics(
  title = "Connect Analytics Sandbox",
  header_width = 400,
  user = user
)
