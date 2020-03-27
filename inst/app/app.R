
# remotes::install_github("tbradley1013/connectAnalytics")
library(connectAnalytics)

shinyApp(
  ui = connectAnalytics:::ca_ui(), 
  server = connectAnalytics:::ca_server
)