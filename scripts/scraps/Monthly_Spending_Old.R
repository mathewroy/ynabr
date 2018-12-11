## Title: YNAB for R
## Author: Mathew Roy
## Comment: Displays monthly spending amounts between a start and end date, 
##          for budget categories of interest and 
##          compares it to historical spending as of the day of the month.
## Created on: November 10, 2018
## Updated on: December 2, 2018

## Clear items from memory if required (uncomment below)
#rm(list=ls(pattern="^searchcriterahere"))

## (Install) and load packages
#install.packages("httr")
require(httr)

#install.packages("jsonlite")
require(jsonlite)

#install.packges("tidyverse)
require(tidyverse)

#install.packages("plotly")
require(plotly)

## Import data using Import_Data.R

## Refresh transactions
df_transactions <- refreshTransactions()

## Plot monthly spending by category and month (as as of day of month)
## Create dataset of interest
plotMonthlyspending <- function(start,end,dayof,categories,interactive = FALSE) {
  start_date_of_int <- as.Date(start)
  end_date_of_int <- as.Date(end)
  day_of_mont_of_int <- dayof
  #day_of_mont_of_int <- 30
  
  ## names of all categories
  list_category_names <- sort(
    trimws(
      gsub("[^[:alnum:][:space:][:punct:]]",
           "",
           unique(df_transactions$category_name))))
  
  ## names of categories of interest to user
  categories_of_int <-
    grep(
      paste0(categories, collapse = "|"),
      list_category_names,
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
    summarize(activity = -1 * sum(amount) / 1E3)
  
  ## Create variables of interest
  meanofinterest = mean(df_of_interest$activity)
  medianofinterest = median(df_of_interest$activity)
  sdofinterest = sd (-1 * df_of_interest$activity)
  
  ## Create plot
  out <- 
    ggplot(df_of_interest, mapping = aes(x = yearmo, y = activity, fill = category_name)) +
    ggtitle(paste0("Monthly Spending as of Day ", day_of_mont_of_int, ": ", paste0(categories_of_int,collapse = ", "))) + 
    geom_col() +
    geom_hline(yintercept = meanofinterest - (1 * sdofinterest)) +
    geom_hline(yintercept = medianofinterest) +
    #geom_hline(yintercept = meanofinterest) +
    geom_hline(yintercept = meanofinterest + (1 * sdofinterest)) +
    geom_text(aes(x = min(yearmo),y = meanofinterest - (1 * sdofinterest), label = "-1 S.D.: ", vjust = -1.0)) +
    geom_text(aes(x = min(yearmo),y = medianofinterest, label = "Median: ", vjust = -1.0)) +
    #geom_text(aes(x = min(yearmo),y = meanofinterest, label = "Mean: ", vjust = -0.5)) +
    geom_text(aes(x = min(yearmo),y = meanofinterest + (1 * sdofinterest), label = "+1 S.D.: ", vjust = -2.0)) +
    theme(axis.text.x = element_text(angle = 45)) +
    scale_fill_discrete(name = "Category") +
    scale_x_discrete(name = "Year-Month") +
    scale_y_continuous(name = "Spending",
                       labels = scales::dollar,
                       breaks = sort(
                         c(
                           seq(0, ceiling(max(df_of_interest$activity) / 10) * 10, length.out = 2), 
                           meanofinterest - (1 * sdofinterest),
                           #meanofinterest,
                           medianofinterest,
                           meanofinterest + (1 * sdofinterest)
                           )
                         )
                       )
  if (interactive == TRUE) {
    out <- ggplotly(out, tooltip = c("activity","yearmo","category_name"))
  }
  return(out)  
}

## Create a plot (e.g.:)
plotMonthlyspending(start = "2018-01-01",
                    end = as.Date("2018-12-31"),
                    dayof = 8,
                    categories = c("Gas"),
                    interactive = FALSE)

start <- "2018-01-01"
end <- "2018-12-31"
day_of_mont_of_int <- 9
categories_of_int <-  c("AirVPN - 10")
df_of_interest <- df_transactions %>%
  filter(
    date >= as.Date(start),
    date <= as.Date(end),
    dayofmonth <= as.integer(day_of_mont_of_int),
    category_name %in% categories_of_int
  ) %>%
  group_by(category_name, yearmo) %>%
  summarize(activity = -1 * sum(amount) / 1E3)
View(df_of_interest)

yearmo <-  strftime(seq(as.Date(start),as.Date(end),by=("1 month")),"%y-%m")
df <- as.data.frame(yearmo,col.names=c("mat"))
View(df)

dfc <- as.data.frame(categories_of_int)
View(dfc)

merge(df,dfc, all = TRUE)
df3 <- merge(x = df, y = df_of_interest, by = "yearmo", all.x = TRUE)
View(df3)

df4 <- df3[df3$activity!=0,]
View(df4)
