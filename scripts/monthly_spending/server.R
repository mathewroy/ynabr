## Load packages
packages <- c("devtools","dplyr","htmltools","httr","jsonlite",
              "magrittr","plotly","shiny","tidyr")
sapply(packages, require, character.only = T)
  
## Loading ynabr fucntions (development)
#function_files <- dir(path = "R\\", pattern = "*.R$", full.names = T)
#lapply(function_files, source, echo = F, print.eval = F, verbose = F)

library(ynabr)

# Load required functions
source("uifunctions.R")

## Server functions
function(input, output) {
  ## Activity stats from data frame of interest
  myreactivevals <- reactiveValues()
  
  mybudget <- reactive({
    auth_token <<- input$ip_token
    df_budgets <<- getStartingData("budgets")
  })
  
  observeEvent(input$ip_entertoken, {
    myreactivevals$nameid <- mybudget() %>% select(id, name)
    myreactivevals$selection <- mybudget() %>% select(name)
  })
  
  output$op_budgetlist <- renderUI({ 
    selectInput(inputId = "ip_budget", label = "Select a budget", choices = myreactivevals$selection)
  })
  
  observeEvent(input$ip_dltransactions, {
    myreactivevals$selected <- input$ip_budget
    budget_name_id <<- myreactivevals$nameid %>% as.data.frame %>% 
      filter(name ==  myreactivevals$selected) %>% as.character()
    df_categories <<- getBudgetDetails("categories")
    df_transactions <<- getBudgetDetails("transactions")
    myreactivevals$categories <- sort(unique(df_transactions$category_name))
    myreactivevals$mindate <- min(df_transactions$date)
    myreactivevals$maxdate <- max(df_transactions$date)
  })

  output$op_categories <- renderUI({ 
    selectInput(inputId = "ip_categories", choices = myreactivevals$categories,
                label = "Category", selected = "Dining Out", multiple = TRUE)
  })
  
  output$op_dateranges <- renderUI({
    ## date range input
    dateRangeMonthsInput(inputId = "ip_daterange", label = "Time frame", 
                         start = "2019-01-01", end = myreactivevals$maxdate, 
                         min = myreactivevals$mindate, max = myreactivevals$maxdate, 
                         minviewmode = "months", format = "yy-mm", startview = "year")
    
  })
  
  ## Activity stats from data frame of interest
  activity_stats <- reactiveValues()
  
  ## Data frame of interest
  df_of_interest <- reactive({
    ## Create an empty dataset with the months and categories of interest
    yearmo <- strftime(seq(lubridate::floor_date(input$ip_daterange[1], "month"),
                           lubridate::ceiling_date(input$ip_daterange[2], "month") - 1,
                           by = ("1 month")),
                       "%y-%m")
    
    category_name <- input$ip_categories
    
    df_of_interest_01 <- merge(as.data.frame(yearmo), as.data.frame(category_name), all = TRUE)
    
    ## Get activity data for the months and categories of interest
    df_of_interest_02 <- df_transactions %>% 
      filter(category_name %in% input$ip_categories, 
             date >= lubridate::floor_date(input$ip_daterange[1], "month"),
             date <= lubridate::ceiling_date(input$ip_daterange[2], "month") - 1,
             dayofmonth <= as.integer(input$ip_day)) %>%
      group_by(category_name, yearmo) %>%
      summarize(activity = -1 * sum(amount) / 1E3)
    
    ## Merge the two datasets on two identifiers that exists in both datasets
    df_of_interest <- merge(x = df_of_interest_01, 
                            y = df_of_interest_02,
                            by = c("yearmo", "category_name"), 
                            all.x = TRUE)
    
    ## Replace months and categories with missing values for activity to zero
    df_of_interest$activity[is.na(df_of_interest$activity)] <- 0
    
    ## Calculate statistics of interest
    if (input$ip_exclude == TRUE) {
      df_of_interest_stats <- df_of_interest[df_of_interest$activity != 0,]
    } else {
      df_of_interest_stats <- df_of_interest
    }
    
    df_of_interest_stats <- df_of_interest_stats %>% 
      group_by(yearmo) %>% summarize(activity = sum(activity))
    
    activity_stats$mean <-  mean(df_of_interest_stats$activity, na.rm = TRUE)
    activity_stats$median <- median(df_of_interest_stats$activity, na.rm = TRUE)
    activity_stats$min <- min(df_of_interest_stats$activity, na.rm = TRUE)
    activity_stats$max <- max(df_of_interest_stats$activity, na.rm = TRUE)
    
    if (nrow(na.omit(df_of_interest_stats)) >= 2) {
      activity_stats$sd <- sd(df_of_interest_stats$activity, na.rm = TRUE)
    } else {
      activity_stats$sd <- 0
      
    }
    
    return(df_of_interest)
  })
  
  ## Create the output table
  output$table <- renderDataTable(df_of_interest())
  
  ## Create output plot
  output$plotly <- renderPlotly({
    ## Formatting for text annotations for statistics
    annotations = list(
      xref = "yearmo",  x = ~ yearmo[1],  xanchor = 'right',  yanchor = 'middle',
      font = list(family = 'Arial', size = 10, color = 'rgba(67,67,67,1)'),
      showarrow = FALSE
    )
    
    ## Formatting for horizontal lines
    hlinefont <- list(color = 'rgba(0,0,0, 1)', width = 2)
    
    ## Create plot
    p1 <- plot_ly(data = df_of_interest(), x =  ~ yearmo, y =  ~ activity,
                  type = 'bar', name =  ~ category_name, color =  ~ category_name) %>%
      layout(barmode = 'stack') %>%
      ## Line and text for Median
      add_segments(x =  ~ yearmo[1], xend = ~ yearmo, showlegend = FALSE, 
                   line = hlinefont, name = "Median", 
                   y = activity_stats$median, yend = activity_stats$median) %>%
      layout(annotations = c(annotations, 
                             list(text = paste('Median'), y = (activity_stats$median))))
    
    if (activity_stats$sd >= 5) {
      p1 <-   p1 %>%
          ## Line and text for Mean - 1 SD
          add_segments(x = ~ yearmo[1], xend = ~ yearmo, showlegend = FALSE,
                       line = hlinefont, name = "Average - 1 SD", 
                       y = (activity_stats$mean - activity_stats$sd),
                       yend = (activity_stats$mean - activity_stats$sd)) %>%
          layout(annotations = c(annotations, 
                                 list(text = paste('Average - 1 SD'),
                                      y = (activity_stats$mean - activity_stats$sd)))) %>%
          ## Line and text for Mean + 1 SD
          add_segments(x = ~ yearmo[1], xend = ~ yearmo, showlegend = FALSE, 
                       line = hlinefont, name = "Average + 1 SD", 
                       y = (activity_stats$mean + activity_stats$sd),
                       yend = (activity_stats$mean + activity_stats$sd)) %>%
          layout(annotations = c(annotations, 
                                 list(text = paste('Average + 1 SD'),
                                      y = (activity_stats$mean + activity_stats$sd))))
      }
  
    ## Other layout features
    p1 %>% 
      layout(
        xaxis = list(title = "Year-month", tickangle = 315),
        yaxis = list(title = "Net spending", tickprefix = "$",
                     range = c(0, max(100,  ceiling(activity_stats$max / 50) * 50)))
    )
    
  })
}