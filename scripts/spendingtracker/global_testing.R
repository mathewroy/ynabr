## Author: Mathew Roy
## Updated on: February 10, 2020
## With help from sources acknowledged below

# Rename to global.R prior to deployment
# Source: Modified from Hadley's github-oauth example script

if (interactive()) {
  options(shiny.port = 9999)
  APP_URL <- "http://127.0.0.1:9999"
} else {
  APP_URL <- "https://flash.shinyapps.io/spendingtracker"
}

my_client_id <- ""
my_secret <- ""

app <- httr::oauth_app(appname = "Month-to-day spending tracker app for YNAB",
                       key = my_client_id,
                       secret = my_secret,
                       redirect_uri = "https://flash.shinyapps.io/spendingtracker"
)

api <- httr::oauth_endpoint(request = NULL, 
                            authorize = paste0("https://app.youneedabudget.com/oauth/authorize"),
                            access = paste0("https://app.youneedabudget.com/oauth/token"))

scope <- "read-only"

has_auth_code <- function(params) {
  return(!is.null(params$code))
}