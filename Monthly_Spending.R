## Title: YNAB for R
## Author: Mathew Roy
## Comment: Import and analyze YNAB data
## Created on: November 10, 2018
## Updated on: November 28 2018

## Clear items from memory if required (uncomment below)
#rm(list=ls(pattern="^searchcriterahere"))

## (Install) and load packages
#install.packages("httr")
require(httr)

#install.packages("jsonlite")
require(jsonlite)

#install.packges("tidyverse)
require(tidyverse)

## Import data using Import_Data.R

## Refresh transactions
df_transactions <- refreshTransactions()

## Plot monthly spending by category and month (as as of day of month)
## Create dataset of interest
plotMonthlyspending <- function(start,end,categories) {
  start_date_of_int <- as.Date(start)
  end_date_of_int <- as.Date(end)
  day_of_mont_of_int <- lubridate::day(end)
  #day_of_mont_of_int <- 30
  categories_of_int <-
    grep(
      paste0(categories, collapse = "|"),
      unique(df_transactions$category_name),
      value = TRUE,
      ignore.case = TRUE
    )
  
  df_of_interest <- df_transactions %>%
    filter(
      date >= start_date_of_int,
      date <= end_date_of_int,
      dayofmonth <= day_of_mont_of_int,
      category_name %in% categories_of_int
    ) %>%
    group_by(category_name, yearmo) %>%
    summarize(activity = sum(amount))
  
  ## Create variables of interest
  meanofinterest = mean(-1 * df_of_interest$activity / 1E3)
  medianofinterest = median(-1 * df_of_interest$activity / 1E3)
  sdofinterest = sd (-1 * df_of_interest$activity / 1E3)
  
  ## Create plot
  out <- 
    ggplot(df_of_interest, mapping = aes(x = yearmo, y = -1 * activity / 1E3)) +
    ggtitle(paste0("Monthly Spending: ", paste0(categories_of_int,collapse = ", "))) + 
    geom_col() +
    geom_hline(yintercept = meanofinterest - (1 * sdofinterest)) +
    geom_hline(yintercept = medianofinterest) +
    #geom_hline(yintercept = meanofinterest) +
    geom_hline(yintercept = meanofinterest + (1 * sdofinterest)) +
    geom_text(aes(x = min(yearmo),y = meanofinterest - (1 * sdofinterest), label = "-1 S.D.: ", vjust = -0.5)) +
    geom_text(aes(x = min(yearmo),y = medianofinterest, label = "Median: ", vjust = -0.5)) +
    #geom_text(aes(x = min(yearmo),y = meanofinterest, label = "Mean: ", vjust = -0.5)) +
    geom_text(aes(x = min(yearmo),y = meanofinterest + (1 * sdofinterest), label = "+1 S.D.: ", vjust = -0.5)) +
    theme(axis.text.x = element_text(angle = 45)) +
    scale_x_discrete(name = "Year-Month") +
    scale_y_continuous(name = "Spending",
                       labels = scales::dollar,
                       breaks = sort(
                         c(
                           seq(0, ceiling(max(-1 * df_of_interest$activity / 1E3) / 10) * 10, length.out = 2), 
                           meanofinterest - (1 * sdofinterest),
                           #meanofinterest,
                           medianofinterest,
                           meanofinterest + (1 * sdofinterest)
                           )
                         )
                       )
  return(out)  
}

plotMonthlyspending(start = "2018-01-01",
                    end = "2018-11-30",
                    categories = c("Gas"))


?paste
