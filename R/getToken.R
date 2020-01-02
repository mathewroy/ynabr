#' A function that asks the user to select the budget of interest
#' @keywords getToken
#' @examples
#' auth_token <- getToken()
getToken <- function() {
  
  print("Enter the personal access token associated with your YNAB account:")
  
  token <- readline(prompt = "Enter token: ") %>% as.character()
  
  if (!is.character(token)) {
    stop("Input an integer")
  }
  
  return(token)
}
