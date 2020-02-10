#' Updates the YNAB transaction data
#' 
#' Adds new transactions, deletes exisiting or modifies existing transactions.
#' If there is no df_transactions data frame, it will create one.
#' Caution: Must be careful with use, as to not overload YNAB's servers.
#' 
#' @name refreshTransactions
#' @keywords refreshTransactions
#' @param param.token.code Your YNAB API personal access token
#' @param param.budgetid The ID associated with the selected budget
#' @param param.dfname (optional) Name of data frame which contains existing transactions
#' @export
#' @import dplyr
#' @importFrom lubridate day
#' @importFrom rlang .data

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".","df_transactions"))

# Version: 0.0.1.0000
# refreshTransactions <- function() {
#   ## If the df_transactions data frame exist, then find out the
#   ## last server knowledge value.
#   ## Then get a data frame (...delta) with the updated or new transactions
#   ## Fix the column names using removeColumnprefix()
#   ## Append it to the the existing transactions data frame, sort id (asc.)
#   ## and date (desc.) form, keep only unique transactions based on id
#   ## (if there are duplicate records due to updates being added, this will
#   ## keeps only the latest transaction since it was sorted by date descending).
#   ## Remove any transactions which were deleted since df_transactions was
#   ## originally created.
#   ## If df_transactions doesn't exit, download all of the transaction data.
#   
#   if (exists("budget_name_id") == FALSE) {
#     budget_name_id <<- selectBudget()
#   }
#   
#   basepoint <- c("https://api.youneedabudget.com/v1")
#   trans_url <- paste0(basepoint, "/budgets/", budget_name_id[1], "/transactions")
#   
#   if (exists("df_transactions") == TRUE) {
#     
#     transactions_sk <- as.integer(max(df_transactions$server_knowledge))
#     
#     new_trans_url <- paste0(trans_url, "?last_knowledge_of_server=", as.integer(transactions_sk + 1))
#     
#     print(paste0("Getting new transactions from: ", new_trans_url))
#     
#     df_transactions_updated <-
#       tryCatch({
#         df_transactions_delta <- getYNAB(new_trans_url) %>% removeColumnprefix() %>% 
#           mutate(date = as.Date(date, "%Y-%m-%d"),
#                  yearmo = strftime(date, "%y-%m"),
#                  dayofmonth = lubridate::day(as.Date(date, "%Y-%m-%d")),
#                  category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", .data$category_name))) %>% 
#           arrange(desc(date))
#         
#         print("Adding it to existing transaction dataset...")
#         df_transactions_updated <-
#           rbind(df_transactions, df_transactions_delta) %>%
#           arrange(id, desc(date)) %>% 
#           distinct(id, .keep_all = TRUE) %>%
#           filter(.data$deleted == FALSE) %>% 
#           arrange(desc(date))
#         return(df_transactions_updated)
#       },
#       warning = function(cond) {
#         df_transactions_updated <- df_transactions
#         message("Error. No new transactions to add.")
#         message(cond)
#         return(df_transactions_updated)
#       })
#   } else {
#     print("df_transactions does not exist. Getting it now..")
#     getBudgetDetails("transactions")
#   }
# }

# Version: 0.1.1.0000
# Added new parameter for token environment, rename param.token to param.token.code and mytoken to mytoken.code
# Version: 0.1.0.0000
# Added parameters for token, budget, exisiting transaction data frame name
refreshTransactions <- function(param.token.code, param.token.env, param.budgetid, param.dfname) {
  
  mytoken.code <- param.token.code
  mytoken.env <- param.token.env
  basepoint <- c("https://api.youneedabudget.com/v1")
  mybudgetid <- param.budgetid
  trans_url <- paste0(basepoint, "/budgets/", mybudgetid, "/transactions")
  
  if (missing(param.dfname) == FALSE) {
    df_transactions <- param.dfname
    
    transactions_sk <- as.integer(max(df_transactions$server_knowledge))
    
    new_trans_url <- paste0(trans_url, "?last_knowledge_of_server=", as.integer(transactions_sk + 1))
    
    print(paste0("Getting new transactions from: ", new_trans_url))
    
    df_transactions_updated <-
      tryCatch({
        df_transactions_delta <- getYNAB(param.url = new_trans_url, param.token.code = mytoken.code, param.token.env = mytoken.env) %>% 
          removeColumnprefix() %>% 
          mutate(.data = .,
                 date = as.Date(.data$date, "%Y-%m-%d"),
                 yearmo = strftime(.data$date, "%y-%m"),
                 dayofmonth = lubridate::day(.data$date),
                 category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", .data$category_name))) %>% 
          arrange(.data = ., desc(.data$date))
        
        print("Adding it to existing transaction dataset...")
        df_transactions_updated <-
          rbind(df_transactions, df_transactions_delta) %>%
          arrange(.data = ., .data$id, desc(.data$date)) %>% 
          distinct(.data = ., .data$id, .keep_all = TRUE) %>%
          filter(.data = ., .data$deleted == FALSE) %>% 
          arrange(.data = ., desc(.data$date))
        return(df_transactions_updated)
      },
      error = function(cond) {
        df_transactions_updated <- df_transactions
        message("No new transactions to add.")
        return(df_transactions_updated)
      })
  } else {
    print("df_transactions does not exist. Getting it now..")
    getBudgetDetails(i = "transactions", param.token.code = mytoken.code, param.token.env = mytoken.env, param.budgetid = mybudgetid)
  }
}
