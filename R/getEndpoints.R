#' Retrieve endpoints
#' 
#' Returns the YNAB primary endpoints as a data frame
#' @name getEndpoints
#' @keywords getEndpoints
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
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
    )
  df2 <- tibble(ep = df, urls = c(lapply(df, function(x)
      paste0("/", x))))
  return(df2)
}
