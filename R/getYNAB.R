#' Connects to YNAB API and converts JSON data to R data frames
#' JSON content > text format > list format > R data frames
#' Caution: Must be careful with use, as to not overal YNAB's servers
#' @param YNAB.url Character vector of the endpoint URL of interest
#' @keywords getYNAB
#' @examples
#' df <- getYNAB("https://api.youneedabudget.com/v1/user") 
getYNAB <- function(YNAB.url) {
  df_json <-
    httr::GET(url = YNAB.url,
              add_headers(Authorization = glue::glue('bearer {auth_token}'))) %>%
    content(as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    as.data.frame()
  
  return(df_json)
}
