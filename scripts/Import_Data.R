## Title: YNAB for R
## Author: Mathew Roy
## Comment: Import and analyze YNAB data
## Created on: November 10, 2018
## Updated on: December 1, 2018

## Clear items from memory if required (uncomment below)
rm(list = ls())
#rm(list=ls(pattern="^searchcriterahere"))

## (Install) and load packages
#install.packages("httr")
require(httr)

#install.packages("jsonlite")
require(jsonlite)

#install.packges("tidyverse)
require(tidyverse)

## Authorizatin token code
## Set full path of a one-lined .txt file containing token
token_txt_file <- "token.txt"
auth_token <- readChar(con = token_txt_file, 
                       nchars = file.info(token_txt_file)$size)
## OR set it within the script
#auth_token <- "12342424242424242"

## Basepoint of YNAB's API
basepoint <- c("https://api.youneedabudget.com/v1")

## Create URL segments for each endpoint
## Note: This data frame is not used anywhere else.
df_ep <- 
  c("user", "budgets", "accounts", "categories", "payees", "payee_locations", 
    "months", "transactions", "scheduled_transactions") %>%
  tibble(ep = ., urls = c(lapply(., function(x) paste0("/", x))))

## Transpose the df_ep data frame
tempvar_name <- df_ep$ep
df_ep <- df_ep[, -1] %>% t() %>% as.data.frame()
colnames(df_ep) <- tempvar_name
rm(tempvar_name)

## Function that will import data from YNAB's api
getYNAB <- function(YNAB.url) {
  ## Get the JSON content, convert to text format, transform it to a 
  ## list format and convert to a data frame
  df_json <-
    httr::GET(url = YNAB.url,
              add_headers(Authorization = glue::glue('bearer {auth_token}'))) %>%
    content(as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    as.data.frame()
  
  return(df_json)
}

## Function that will remove the prefix from the df's column name
removeColumnprefix <- function(x) {
  ## Retrieve the name of the input parameter and treat it as a string,
  ## not an object.
  ## Retrieve the column names that starts with "data."
  ## The assumption is that the name will follow the structure "data.{x}.something"
  ## Locate where the second period is in the name, if there is one.
  ## Otherwise, locate the first period if there is one.
  ## Otherwise recognize that there is no period.
  ## Retrieve only the text after the period of interest
  ## eg.  "data.{x}.something" > "something" or
  ##      "data.something" > "something" or
  ##      "something" > "something"
  ## Return a list of new column names to lapply
  ## Assign the new column names to the columns that start with "data."
  ## Return a data frame with the new column names
  df_name <- deparse(substitute(x))
  
  col_oldnames <- colnames(x)[grepl("^data.", colnames(x))]
  
  list_col_newnames <- lapply(col_oldnames, function(y) {
    
    period_locs <- which(strsplit(y, "")[[1]] == ".")
    
    if (length(period_locs) > 1) {
      period_ofinterest_loc = period_locs[2]
    } else if (length(period_locs) == 1) {
      period_ofinterest_loc = period_locs[1]
    } else {
      period_ofinterest_loc = 0
    }
    
    col_newname <- substring(y, period_ofinterest_loc + 1)
    return(col_newname)
  })
  
  colnames(x)[grepl("^data.", colnames(x))] <- list_col_newnames
  return(as.data.frame(x))
}

## Create URLs for each endpoint and import data
for (i in c("user", "budgets")) {
  print(paste0("Getting data from: ", basepoint, "/", i))
  assign(paste0("df_", i), getYNAB(paste0(basepoint, "/", i)))
  assign(paste0("df_", i), removeColumnprefix(get(paste0("df_", i))))
}
rm(i)

## Input name of the budget
#name_budget <- c("My Budget")
getBudget <- function() {
  ## Print a list of the budget names
  ## Ask user to input the number of the budget corresponding to place on list
  ## Convert input to integer
  ## If it isn't an integer greater than 0 then exit.
  ## Based on the input, return the correspoding Budget Name and ID
  print(glue::glue('Enter the number associated with the budgest of interest:'))
  print(as.list(df_budgets$name))
  
  budget_no_user_input <- readline(prompt = "Enter budget number: ") %>%
    as.integer()
  
  if(!is.integer(budget_no_user_input))
    stop("Input an integer")
  
  if(budget_no_user_input < 0)
    stop("Input should be an integer greater than/equal to 1")
 
   name_budget <- as.character(df_budgets$name[budget_no_user_input])
  
   id_budget <- df_budgets %>% filter(name == name_budget) %>% select(id) %>%
    as.character()
  
   return(c(id_budget,name_budget))
}

## Get budget name and ID
budget_name_id <- getBudget()

## Create URLs for each endpoint and import data
#df_transactions1 <- df_transactions
for (i in c("accounts", "categories", "months", "payees", "transactions")) {
  print(paste0(basepoint, "/budgets/", budget_name_id[1], "/", i))
  assign(paste0("df_", i), getYNAB(paste0(
    basepoint, "/budgets/", budget_name_id[1], "/", i
  )))
  assign(paste0("df_", i), removeColumnprefix(get(paste0("df_", i))))
  
  ## Get detailed category names
  if (i == "categories") {
    df_categories <- df_categories %>%
      rename(subcategories = categories)
    df_subcategories <-
      lapply(df_categories$subcategories, as.data.frame) %>%
      bind_rows
  }
  
  ## Reformat month column in df_month
  if (i == "months") {
    df_months <- df_months %>%
      mutate(month = lubridate::date(as.Date(month, "%Y-%m-%d")),
             yearmo = strftime(month, "%y-%m"))
  }
  
  ## Reformat transaction dates
  if (i == "transactions") {
    df_transactions <- df_transactions %>%
      mutate(
        date = as.Date(date, "%Y-%m-%d"),
        yearmo = strftime(date, "%y-%m"),
        dayofmonth = lubridate::day(as.Date(date, "%Y-%m-%d")),
        category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", category_name))
      )  %>%
      arrange(desc(date))
  }
}
rm(i)

## Create URL (based on API's structure) to get updated transactions data
## and add new transactions
## Get the delta (last point of server interaction)
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
    
    new_trans_url <-
      paste0(trans_url, "?last_knowledge_of_server=", as.integer(transactions_sk + 1))
    
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
        return(df_transactions_updated %>% 
                 mutate(date = as.Date(date, "%Y-%m-%d"), yearmo = strftime(date, "%y-%m"),
                        dayofmonth = lubridate::day(as.Date(date, "%Y-%m-%d")),
                        category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", category_name))) %>%
                 arrange(desc(date)))
      },
      error = function(cond) {
        df_transactions_updated <- df_transactions
        message("Error. No new transactions to add.")
        message(cond)
        return(df_transactions_updated)
      },
      warning = function(cond) {
        message("warning")
        message(cond)
        return(NULL)
      })
  } else {
    print(paste0("Getting new transactions from: ", trans_url))
    df_transactions_updated <-
      getYNAB(paste0(basepoint, "/budgets/", budget_name_id[1], "/transactions")) %>%
      removeColumnprefix()
    return(df_transactions_updated %>% 
             mutate(date = as.Date(date, "%Y-%m-%d"), yearmo = strftime(date, "%y-%m"),
                    dayofmonth = lubridate::day(as.Date(date, "%Y-%m-%d")),
                    category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", category_name))) %>%
             arrange(desc(date)))
  }
}

df_transactions <- refreshTransactions()

