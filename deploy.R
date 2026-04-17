library(rsconnect)

# token and secret created in server on Apr 17, 2026
setAccountInfo(name = Sys.getenv("SHINYAPPS_ACCOUNT"),
               token = Sys.getenv("SHINYAPPS_TOKEN"),
               secret = Sys.getenv("SHINYAPPS_SECRET"))

deployApp(account = Sys.getenv("SHINYAPPS_ACCOUNT"),
          appName = "forecasting-cf",
          appTitle = "forecasting-cf",
          forceUpdate = TRUE)