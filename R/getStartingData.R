#' Retrieves user or budget names
#' 
#' Gets the following YNAB data for a YNAB subscriber:
#'  "user", "budgets"
#' @name getStartingData
#' @param i name of endpoint
#' @param param.token Your YNAB API personal access token
#' @keywords getStartingData
#' @export
#' @import dplyr
#' @examples
#' endpoint <- "budgets"
#' mytoken <- "1234567890ABCDE"
#' df_budgets <- getStartingData(i = endpoint, param.token = mytoken) 

# Version: 0.0.1.0000
# getStartingData <- function(i) {
#   basepoint <- c("https://api.youneedabudget.com/v1")
#   
#   if (!(i %in% c("user", "budgets"))) {
#     stop("Please enter the arguments 'user' or 'budgets'.")
#   }
#   print(paste0("Getting data from: ", basepoint, "/", i))
#   df <- getYNAB(paste0(basepoint, "/", i)) %>% 
#     removeColumnprefix()
#   return(df)
# }

# Version: 0.1.1.0000
# Added new parameter for token environment, rename param.token to param.token.code and mytoken to mytoken.code
# Version: 0.1.0.0000
# Added new parameter for token
getStartingData <- function(i, param.token.code, param.token.env) {
  basepoint <- c("https://api.youneedabudget.com/v1")
  
  if (!(i %in% c("user", "budgets"))) {
    stop("Please enter the arguments 'user' or 'budgets'.")
  }
  
  myurl <- paste0(basepoint, "/", i)
  mytoken.code <- param.token.code
  mytoken.env <- param.token.env
  
  print(paste0("Getting data from: ", myurl))
  
  df <- getYNAB(param.url = myurl, param.token.code = mytoken.code, param.token.env = mytoken.env) %>% removeColumnprefix()
  return(df)
}
