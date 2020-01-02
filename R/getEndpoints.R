#' Retrieve endpoints
#' 
#' Returns the YNAB primary endpoints as a data frame
#' 
#' @keywords getEndpoints
#' @export
#' 
#' @examples
#' getEndpoints()
## Create URL segments for each endpoint
getEndpoints <- function() {
  df <-
    c(
      "user",
      "budgets",
      "accounts",
      "categories",
      "payees",
      "payee_locations",
      "months",
      "transactions",
      "scheduled_transactions"
    ) %>%
    tibble(ep = ., urls = c(lapply(., function(x)
      paste0("/", x))))
  return(df)
}