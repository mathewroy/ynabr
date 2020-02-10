#' Get budget details
#' 
#' Gets the following YNAB data for a pre-specified budget:
#'  "accounts", "categories", "months", "payees", "payee_locations", 
#'  "subcategories", "scheduled_transactions", "transactions".
#' Caution: Must be careful with use, as to not overload YNAB's servers.
#' 
#' @name getBudgetDetails
#' @param i name of endpoint (one of: "accounts", "categories", "months", "payees", "payee_locations", "subcategories", "scheduled_transactions", "transactions")
#' @param param.token Your YNAB API personal access token
#' @param param.budgetid The ID associated with the selected budget
#' @keywords getBudgetDetails
#' @export
#' @import dplyr
#' @importFrom lubridate  date day
#' @importFrom rlang .data
#' @examples
#' #token location
#' library(dplyr)
#' mytoken <- "1234567890ABCDE"
#' df_budgets <- getStartingData(i = "budgets", param.token = mytoken)
#' mybudgetid <- df_budgets[df_budgets$name == "My Budget",c("id")]
#' #OR
#' #mybudgetid <- selectBudget(param.token = mytoken)
#' df_accounts <- getBudgetDetails(i = "accounts", param.token = mytoken, 
#' param.budgetid = mybudgetid)

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".","budget_name_id"))

# Version: 0.0.1.0000
# getBudgetDetails <- function(i) {
#   
#   if (exists("budget_name_id") == FALSE) {
#     budget_name_id <<- selectBudget()
#   }
#   
#   valid_i <-  c("accounts", "categories", "months", "payees", "payee_locations", 
#                 "subcategories", "scheduled_transactions", "transactions")
#   
#   basepoint <- c("https://api.youneedabudget.com/v1")
#   
#   k  <-  ""
#   
#   if (!(i %in% valid_i)) {
#     stop(paste0(c("Argument must be one of: ", valid_i), collapse = " ")) 
#   }
#   
#   if (i == "subcategories") {
#     i = "categories"
#     k = "subcategories"
#   }
#   
#   print(paste0(basepoint, "/budgets/", budget_name_id[1], "/", i))
#   df <- getYNAB(paste0(basepoint, "/budgets/", budget_name_id[1], "/", i)) %>% 
#     removeColumnprefix()
#   
#   if (i == "categories") {
#     df <- rename(.data = df, subcategories = .data$categories)
#   } else if (i == "months") {
#     df <- df %>% mutate(month = lubridate::date(as.Date(.data$month, "%Y-%m-%d")),
#                         yearmo = strftime(.data$month, "%y-%m"))
#   } else if (i == "transactions") {
#     
#     df <- df %>%
#       mutate(
#         date = as.Date(.data$date, "%Y-%m-%d"),
#         yearmo = strftime(.data$date, "%y-%m"),
#         dayofmonth = lubridate::day(as.Date(.data$date, "%Y-%m-%d")),
#         category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", x = .$category_name))
#       )
#     
#     df <- getUnsplit(df)
#     
#   }
#   
#   if (k == "subcategories") {
#     df <- lapply(df$subcategories, as.data.frame) %>% bind_rows %>%
#       mutate(name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", x = .$name)))
#   }
#   
#   return(df)
# }

# Version: 0.1.1.0000
# Added new parameter for token environment, rename param.token to param.token.code and mytoken to mytoken.code
# Version: 0.1.0.0000
# Added new parameter for token
# Added new parameter for budget id
getBudgetDetails <- function(i, param.token.code, param.token.env, param.budgetid) {
  
  valid_i <-  c("accounts", "categories", "months", "payees", "payee_locations", 
                "subcategories", "scheduled_transactions", "transactions")
  
  basepoint <- c("https://api.youneedabudget.com/v1")
  
  k  <-  ""
  
  if (!(i %in% valid_i)) stop(paste0(c("Argument must be one of: ", valid_i), collapse = " "))
  
  if (i == "subcategories") {
    i = "categories"
    k = "subcategories"
  }
  
  mybudgetid <- param.budgetid
  myurl <- paste0(basepoint, "/budgets/", mybudgetid, "/", i)
  mytoken.code <- param.token.code
  mytoken.env <- param.token.env
  
  print(myurl)
  
  df <- getYNAB(param.url = myurl, param.token.code = mytoken.code, param.token.env = mytoken.env) %>% removeColumnprefix()
  
  if (i == "categories") {
    df <- rename(.data = df, subcategories = .data$categories)
  } else if (i == "months") {
    df <- df %>% mutate(.data = .,
                        month = lubridate::date(as.Date(.data$month, "%Y-%m-%d")),
                        yearmo = strftime(.data$month, "%y-%m"))
  } else if (i == "transactions") {
    
    df <- df %>%
      mutate(.data = .,
        date = as.Date(.data$date, "%Y-%m-%d"),
        yearmo = strftime(.data$date, "%y-%m"),
        dayofmonth = lubridate::day(.data$date),
        category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", x = .$category_name))
      )
    
    df <- getUnsplit(d= df, param.token.code = mytoken.code, param.token.env = mytoken.env, param.budgetid = mybudgetid)
    
  }
  
  if (k == "subcategories") {
    df <- lapply(df$subcategories, as.data.frame) %>% bind_rows %>%
      mutate(.data = ., 
             name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", x = .$name)))
  }
  
  return(df)
}
