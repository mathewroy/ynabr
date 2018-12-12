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
auth_token <- readChar(con = token_txt_file, nchars = file.info(token_txt_file)$size)
## OR set it within the script
#auth_token <- "12342424242424242"

## Basepoint of YNAB's API
basepoint <- c("https://api.youneedabudget.com/v1")

## Create URL segments for each endpoint
## Note: This data frame is not used anywhere else.
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

## Function that will import data from YNAB's api
getYNAB <- function(YNAB.url, auth_token = auth_token) {
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

## Create URLs for each endpoint and import data.
getStartingData <- function(i) {
  if (!(i %in% c("user", "budgets"))) {
    stop("Please enter the arguments 'user' or 'budgets'.")
  }
    print(paste0("Getting data from: ", basepoint, "/", i))
    df <- getYNAB(paste0(basepoint, "/", i)) %>% 
      removeColumnprefix()
    return(df)
}

## Input name of the budget
#name_budget <- c("My Budget")
selectBudget <- function() {
  
  ## Create df_budgets if it doesn't exist
  if (exists("df_budgets") == FALSE) {
    df_budgets <- getStartingData("budgets")
  }
  
  ## Print a list of the budget names
  ## Ask user to input the number of the budget corresponding to place on list
  ## Convert input to integer
  ## If it isn't an integer greater than 0 then exit.
  ## Based on the input, return the correspoding Budget Name and ID
  print("Enter the number associated with the budgest of interest:")
  print(as.list(df_budgets$name))
  
  input_budget_no <- readline(prompt = "Enter budget number: ") %>%
    as.integer()
  
  if (!is.integer(input_budget_no)) {
    stop("Input an integer")
  }
  
  if (input_budget_no <= 0) {
    stop("Input should be an integer greater than/equal to 1")
  }
  
  budget_name <- as.character(df_budgets$name[input_budget_no])
  
  budget_id <- df_budgets %>% filter(name == budget_name) %>% select(id) %>%
    as.character()
  
  return(c(budget_id, budget_name))
}

## Create URLs for each endpoint and import data
getBudgetDetails <- function(i) {

  valid_i <-  c("accounts", "categories", "months", "payees", "payee_locations", 
              "subcategories", "scheduled_transactions", "transactions")
  k  <-  ""
  
  if (!(i %in% valid_i)) {
    stop(paste0(c("Argument must be one of: ", valid_i), collapse = " ")) 
  }
  
  if (i == "subcategories") {
    i = "categories"
    k = "subcategories"
  }
  
  print(paste0(basepoint, "/budgets/", budget_name_id[1], "/", i))
  df <- getYNAB(paste0(basepoint, "/budgets/", budget_name_id[1], "/", i)) %>% 
    removeColumnprefix()
  
  if (i == "categories") {
    df <- df %>% rename(subcategories = categories)
  } else if (i == "months") {
    df <- df %>% mutate(month = lubridate::date(as.Date(month, "%Y-%m-%d")),
                        yearmo = strftime(month, "%y-%m"))
  } else if (i == "transactions") {
    df_transactions <- df_transactions %>%
      mutate(
        date = as.Date(date, "%Y-%m-%d"),
        yearmo = strftime(date, "%y-%m"),
        dayofmonth = lubridate::day(as.Date(date, "%Y-%m-%d")),
        category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", category_name))
      )  %>%
      arrange(desc(date))
  }

  if (k == "subcategories") {
    df <- lapply(df$subcategories, as.data.frame) %>% bind_rows
  }

  return(df)
}

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

getEndpoints()
df_user <- getStartingData("user")
df_budgets <- getStartingData("budgets")

budget_name_id <- selectBudget()

df_accounts <- getBudgetDetails("accounts")
df_categories <- getBudgetDetails("categories")
df_subcategories <- getBudgetDetails("subcategories")
df_months <- getBudgetDetails("months")
df_payees <- getBudgetDetails("payees")
df_payee_locations <- getBudgetDetails("payee_locations")
df_scheduled_transactions <- getBudgetDetails("scheduled_transactions")
df_transactions <- getBudgetDetails("transactions")
#df_transactions <- refreshTransactions()

