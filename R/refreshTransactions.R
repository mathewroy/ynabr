#' Updates the YNAB transaction data
#' Adds new transactions, deletes exisiting or modifies existing transactions.
#' If there is no df_transactions data frame, it will create one.
#' Caution: Must be careful with use, as to not overal YNAB's servers
#' @keywords refreshTransactions
#' @examples
#' df_transactions <- refreshTransactions()
refreshTransactions <- function() {
  ## If the df_transactions data frame exist, then find out the
  ## last server knowledge value.
  ## Then get a data frame (...delta) with the updated or new transactions
  ## Fix the column names using removeColumnprefix()
  ## Append it to the the existing transactions data frame, sort id (asc.)
  ## and date (desc.) form, keep only unique transactions based on id
  ## (if there are duplicate records due to updates being added, this will
  ## keeps only the latest transaction since it was sorted by date descending).
  ## Remove any transactions which were deleted since df_transactions was
  ## originally created.
  ## If df_transactions doesn't exit, download all of the transaction data.
  trans_url <- 
    paste0(basepoint, "/budgets/", budget_name_id[1], "/transactions")
  
  if (exists("df_transactions") == TRUE) {
    
    transactions_sk <- as.integer(max(df_transactions$server_knowledge))
    
    new_trans_url <- paste0(trans_url, "?last_knowledge_of_server=", as.integer(transactions_sk + 1))
    
    print(paste0("Getting new transactions from: ", new_trans_url))
    
    df_transactions_updated <-
      tryCatch({
        df_transactions_delta <- getYNAB(new_trans_url) %>% removeColumnprefix() %>% 
          mutate(date = as.Date(date, "%Y-%m-%d"),
                 yearmo = strftime(date, "%y-%m"),
                 dayofmonth = lubridate::day(as.Date(date, "%Y-%m-%d")),
                 category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", category_name))) %>% 
          arrange(desc(date))
        
        print("Adding it to existing transaction dataset...")
        df_transactions_updated <-
          rbind(df_transactions, df_transactions_delta) %>%
          arrange(id, desc(date)) %>% 
          distinct(id, .keep_all = TRUE) %>%
          filter(deleted == FALSE) %>% 
          arrange(desc(date))
        return(df_transactions_updated)
      },
      error = function(cond) {
        df_transactions_updated <- df_transactions
        message("Error. No new transactions to add.")
        message(cond)
        return(df_transactions_updated)
      })
  } else {
    print("df_transactions does not exist. Getting it now..")
    getBudgetDetails("transactions")
  }
}