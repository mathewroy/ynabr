## Load packages
packages <- c("dplyr","htmltools","httr","jsonlite",
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
  revals <- reactiveValues()
  
  observeEvent(input$ip_entertoken, {
    revals$mytoken <- input$ip_token
    revals$budgets <- getStartingData(i = "budgets",param.token = revals$mytoken) %>% 
      select(id, name) # reactive required for output$op_budgetlist
    revals$budgetnames <- revals$budgets %>% select(name)
  })
  
  output$op_budgetlist <- renderUI({ 
    selectInput(inputId = "ip_budget", label = "Select a budget", choices = revals$budgetnames)
  })
  
  observeEvent(input$ip_dltransactions, {
    revals$mybudgetid <- revals$budgets %>% filter(name ==  input$ip_budget) %>% select(id) %>% as.character()
  })

  reactive_transactions <- eventReactive(input$ip_dltransactions,{
    
    df_transactions <- getBudgetDetails("transactions",
                                        param.token = revals$mytoken,
                                        param.budgetid = revals$mybudgetid)
    
    revals$mindate <- min(df_transactions$date,na.rm = T)
    revals$maxdate <- max(df_transactions$date, na.rm = T)
    revals$categories <- sort(unique(df_transactions$category_name))
    return(df_transactions)
  },ignoreNULL = T)
  
  output$op_categories <- renderUI({ 
    selectInput(inputId = "ip_categories", choices = revals$categories,
                label = "Category", selected = revals$categories[1], multiple = TRUE)
  })
  
  output$op_dateranges <- renderUI({
    ## date range input
    dateRangeMonthsInput(inputId = "ip_daterange", label = "Time frame", 
                         start = "2019-01-01", end = revals$maxdate, 
                         min = revals$mindate, max = revals$maxdate, 
                         minviewmode = "months", format = "yy-mm", startview = "year")
    
  })
  
  ## Data frame of interest
  reactive_df_subset <- reactive({
    ## Create an empty dataset with the months and categories of interest
    yearmo <- strftime(x = seq(from = lubridate::floor_date(input$ip_daterange[1], "month"),
                               to = lubridate::ceiling_date(input$ip_daterange[2], "month") - 1,
                               by = "1 month"), 
                       format = "%y-%m")
    
    category_name <- input$ip_categories
    
    df_of_interest_01 <- merge(as.data.frame(yearmo), as.data.frame(category_name), all = TRUE)
    
    ## Get activity data for the months and categories of interest
    df_of_interest_02 <- reactive_transactions() %>% 
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
    
    df_of_interest_stats <- df_of_interest_stats %>% group_by(yearmo) %>% summarize(activity = sum(activity))
    
    revals$mean <-  mean(df_of_interest_stats$activity, na.rm = TRUE)
    revals$median <- median(df_of_interest_stats$activity, na.rm = TRUE)
    revals$min <- min(df_of_interest_stats$activity, na.rm = TRUE)
    revals$max <- max(df_of_interest_stats$activity, na.rm = TRUE)
    
    if (nrow(na.omit(df_of_interest_stats)) >= 2) {
      revals$sd <- sd(df_of_interest_stats$activity, na.rm = TRUE)
    } else {
      revals$sd <- 0
    }
    
    return(df_of_interest)
  })
  
  ## Create the output table
  output$table <- renderDataTable(reactive_df_subset())
  
  ## Create output plot
  output$plotly <- renderPlotly({
    tryCatch({
    ## Formatting for text annotations for statistics
    annotations = list(
      xref = "yearmo",  x = ~ yearmo[1],  xanchor = 'right',  yanchor = 'middle',
      font = list(family = 'Arial', size = 10, color = 'rgba(67,67,67,1)'),
      showarrow = FALSE
    )
    
    ## Formatting for horizontal lines
    hlinefont <- list(color = 'rgba(0,0,0, 1)', width = 2)
    
    ## Create plot
    p1 <- plot_ly(data = reactive_df_subset(), x =  ~ yearmo, y =  ~ activity,
                  type = 'bar', name =  ~ category_name, color =  ~ category_name) %>%
      layout(barmode = 'stack') %>%
      ## Line and text for Median
      add_segments(x =  ~ yearmo[1], xend = ~ yearmo, showlegend = FALSE, 
                   line = hlinefont, name = "Median", 
                   y = revals$median, yend = revals$median) %>%
      layout(annotations = c(annotations, list(text = paste('Median'), y = (revals$median))))
    
    if (revals$sd >= 5) {
      p1 <-   p1 %>%
          ## Line and text for Mean - 1 SD
          add_segments(x = ~ yearmo[1], xend = ~ yearmo, showlegend = FALSE,
                       line = hlinefont, name = "Average - 1 SD", 
                       y = (revals$mean - revals$sd),
                       yend = (revals$mean - revals$sd)) %>%
          layout(annotations = c(annotations, list(text = paste('Average - 1 SD'),
                                      y = (revals$mean - revals$sd)))) %>%
          ## Line and text for Mean + 1 SD
          add_segments(x = ~ yearmo[1], xend = ~ yearmo, showlegend = FALSE, 
                       line = hlinefont, name = "Average + 1 SD", 
                       y = (revals$mean + revals$sd),
                       yend = (revals$mean + revals$sd)) %>%
          layout(annotations = c(annotations,  list(text = paste('Average + 1 SD'),
                                      y = (revals$mean + revals$sd))))
      }
  
    ## Other layout features
    p1 %>% 
      layout(
        xaxis = list(title = "Year-month", tickangle = 315),
        yaxis = list(title = "Net spending", tickprefix = "$",
                     range = c(0, max(100,  ceiling(revals$max / 50) * 50)))
    )
    },
    warning=function(w){
      message(w)
    })  
  })
  
}