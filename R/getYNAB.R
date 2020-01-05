#' Connect to YNAB API
#' 
#' Connects to YNAB API and converts JSON data to R data frames
#' JSON content > text format > list format > R data frames
#' Caution: Must be careful with use, as to not overal YNAB's servers
#' @name getYNAB
#' @param param.url Character vector of the endpoint URL of interest
#' @param param.token Your YNAB API personal access token
#' @keywords getYNAB
#' @export
#' @import dplyr httr jsonlite
#' @importFrom rlang .data
#' @examples
#' url <- "https://api.youneedabudget.com/v1/user"
#' mytoken <- "1234567890ABCDE"
#' df <- getYNAB(param.url = url, param.token = mytoken)
 
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# Version: 0.0.1.0000
# getYNAB <- function(YNAB.url) {
#   
#   if (exists("auth_token") == FALSE) {
#     auth_token <<- getToken()
#   }
#   
#   df_json <-
#     httr::GET(url = YNAB.url,
#               add_headers(Authorization = glue::glue('bearer {auth_token}'))) %>%
#     content(as = "text") %>%
#     jsonlite::fromJSON(flatten = TRUE)
#   
#   
#   if (any(names(df_json[["data"]]) %in% c("user", "budgets"))) {
#     df_json <- df_json[["data"]] %>% .[[1]]
#   } else {
#     df_json <- df_json %>% as.data.frame()
#   }
#   
#   return(df_json)
# }

# Version: 0.1.0.0000
# Added new parameter for token
# Removed use of glue from glue package
# Changed name of final output
getYNAB <- function(param.url, param.token) {
  myurl <- param.url
  mytoken <- param.token

  df_json <-
    httr::GET(url = myurl, add_headers(Authorization = paste("bearer",mytoken))) %>%
    content(as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)
  
  
  if (any(names(df_json[["data"]]) %in% c("user", "budgets"))) {
    df <- df_json[["data"]] %>% .[[1]]
  } else {
    df <- df_json %>% as.data.frame()
  }
  
  return(df)
}
