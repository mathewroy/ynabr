#' Retrieves user or budget names
#' 
#' Gets the following YNAB data for a YNAB subscriber:
#'  "user", "budgets"
#' @param i name of endpoint
#' @keywords getStartingData
#' @export
#' @examples
#' df_user <- getStartingData("user")
#' df_budgets <- getStartingData("budgets") 
getStartingData <- function(i) {
  basepoint <- c("https://api.youneedabudget.com/v1")
  
  if (!(i %in% c("user", "budgets"))) {
    stop("Please enter the arguments 'user' or 'budgets'.")
  }
  print(paste0("Getting data from: ", basepoint, "/", i))
  df <- getYNAB(paste0(basepoint, "/", i)) %>% 
    removeColumnprefix()
  return(df)
}
