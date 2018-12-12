## Title: YNAB for R
## Author: Mathew Roy
## Comment: Import and analyze YNAB data
## Created on: November 10, 2018
## Updated on: December 12, 2018

## Clear items from memory if required (uncomment below)
#rm(list = ls())
#rm(list=ls(pattern="^searchcriterahere"))

## (Install) and load packages
#install.packages("devtools")
library(devtools)

#install.packages("httr")
require(httr)

#install.packages("jsonlite")
require(jsonlite)

#install.packages("tidyverse")
require(tidyverse)

#devtools::install_github("mathewroy/ynabr")
library(ynabR)

## Authorization token code
## Set full path of a one-lined .txt file containing token
token_txt_file <- "token.txt"
auth_token <- readChar(con = token_txt_file, nchars = file.info(token_txt_file)$size)
#auth_token <- "12342424242424242"

## Basepoint of YNAB's API
basepoint <- c("https://api.youneedabudget.com/v1")


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
