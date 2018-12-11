## Title: ynabR: Track Monthly Spending App
## Author: Mathew Roy
## Comment: Import and analyze YNAB data
## Created on: November 5, 2018
## Updated on: December 10, 2018

library(shiny)
library(htmltools)
library(tidyverse)
library(plotly)

## Date range picker (using months only)
## Source: https://stackoverflow.com/a/38974106/9697283
{dateRangeMonthsInput <- {
  function(inputId,
           label,
           start = NULL,
           end = NULL,
           min = NULL,
           max = NULL,
           format = "yyyy-mm-dd",
           startview = "month",
           minviewmode = "months",
           # added manually
           weekstart = 0,
           language = "en",
           separator = " to ",
           width = NULL) {
    # If start and end are date objects, convert to a string with yyyy-mm-dd format
    # Same for min and max
    if (inherits(start, "Date"))
      start <- format(start, "%Y-%m-%d")
    if (inherits(end,   "Date"))
      end   <- format(end,   "%Y-%m-%d")
    if (inherits(min,   "Date"))
      min   <- format(min,   "%Y-%m-%d")
    if (inherits(max,   "Date"))
      max   <- format(max,   "%Y-%m-%d")
    
    htmltools::attachDependencies(
      div(
        id = inputId,
        class = "shiny-date-range-input form-group shiny-input-container",
        style = if (!is.null(width))
          paste0("width: ", validateCssUnit(width), ";"),
        
        controlLabel(inputId, label),
        # input-daterange class is needed for dropdown behavior
        div(
          class = "input-daterange input-group",
          tags$input(
            class = "input-sm form-control",
            type = "text",
            `data-date-language` = language,
            `data-date-weekstart` = weekstart,
            `data-date-format` = format,
            `data-date-start-view` = startview,
            `data-date-min-view-mode` = minviewmode,
            # added manually
            `data-min-date` = min,
            `data-max-date` = max,
            `data-initial-date` = start
          ),
          span(class = "input-group-addon", separator),
          tags$input(
            class = "input-sm form-control",
            type = "text",
            `data-date-language` = language,
            `data-date-weekstart` = weekstart,
            `data-date-format` = format,
            `data-date-start-view` = startview,
            `data-date-min-view-mode` = minviewmode,
            # added manually
            `data-min-date` = min,
            `data-max-date` = max,
            `data-initial-date` = end
          )
        )
      ),
      datePickerDependency
    )
  }
}
  
  `%AND%` <- function(x, y) {
    if (!is.null(x) && !is.na(x))
      if (!is.null(y) && !is.na(y))
        return(y)
    return(NULL)
  }
  
  controlLabel <- function(controlName, label) {
    label %AND% tags$label(class = "control-label", `for` = controlName, label)
  }
  
  ## the datePickerDependency is from https://github.com/rstudio/shiny/blob/master/R/input-date.R
  datePickerDependency <- {
    htmltools::htmlDependency(
      "bootstrap-datepicker",
      "1.6.4",
      c(href = "shared/datepicker"),
      script = "js/bootstrap-datepicker.min.js",
      stylesheet = "css/bootstrap-datepicker3.min.css",
      # Need to enable noConflict mode. See #1346.
      head = "<script>
      (function() {
      var datepicker = $.fn.datepicker.noConflict();
      $.fn.bsDatepicker = datepicker;
      })();
      </script>"
    )
}
}

## Function to add suffix to days
## Source: https://stackoverflow.com/a/40041236/9697283
append_date_suffix <- function(dayy){
  suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                    dayy %% 10 == 1 ~ 'st',
                    dayy %% 10 == 2 ~ 'nd',
                    dayy %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(dayy, suff)
}

## User interface
ui <- {fluidPage(
  sidebarLayout(
    position="right",
    sidebarPanel(
      ## category name input
      {selectInput(
        inputId = "ip_categories",
        choices = sort(unique(df_transactions$category_name)),
        label = "Category",
        selected = "Dining Out",
        multiple = TRUE
      )},
      
      ## day of month input
      {sliderInput(
        inputId = "ip_day",
        min = 1,
        max = 31,
        step = 1,
        value = lubridate::day(Sys.Date()),
        round = TRUE,
        animate = TRUE,
        label = "As of day"
      )},
      
      ## date range input
      {dateRangeMonthsInput(
        inputId = "ip_daterange",
        label = "Time frame",
        start = "2018-01-01",
        end = max(df_transactions$date),
        min = min(df_transactions$date),
        max = max(df_transactions$date),
        minviewmode = "months",
        format = "yy-mm",
        startview = "year"
      )},
      
      ## Exclude months with zero net activity
      {checkboxInput(
        inputId = "ip_exclude",
        label = "Exclude months with net activity of zero in statistics",
        value = FALSE
      )}),
    mainPanel(
      plotlyOutput("plotly")
      #,plotOutput("plot")
      #,dataTableOutput("table")
    )
  )
)}

## Server functions
server <- function(input, output) {
  ## Activity stats from data frame of interest
  activity_stats <- reactiveValues()
  
  ## Data frame of interest
  df_of_interest <- reactive({
    ## Create an empty dataset with the months and categories of interest
    yearmo <-
      strftime(seq(
        lubridate::floor_date(input$ip_daterange[1], "month"),
        lubridate::ceiling_date(input$ip_daterange[2], "month") - 1,
        by = ("1 month")
      ),
      "%y-%m")
    
    category_name <- input$ip_categories
    
    df_of_interest_01 <-
      merge(as.data.frame(yearmo),
            as.data.frame(category_name),
            all = TRUE)
    
    ## Get activity data for the months and categories of interest
    df_of_interest_02 <-
      df_transactions %>% filter(
        category_name %in% input$ip_categories,
        date >= lubridate::floor_date(input$ip_daterange[1], "month"),
        date <= lubridate::ceiling_date(input$ip_daterange[2], "month") - 1,
        dayofmonth <= as.integer(input$ip_day)
      ) %>%
      group_by(category_name, yearmo) %>%
      summarize(activity = -1 * sum(amount) / 1E3)
    
    ## Merge the two datasets on two identifiers that exists in both datasets
    df_of_interest <-
      merge(
        x = df_of_interest_01,
        y = df_of_interest_02,
        by = c("yearmo", "category_name"),
        all.x = TRUE
      )
    
    ## Replace months and categories with missing values for activity to zero
    df_of_interest$activity[is.na(df_of_interest$activity)] <- 0
    
    ## Calculate statistics of interest
    if (input$ip_exclude == TRUE) {
      df_of_interest_stats <- df_of_interest[df_of_interest$activity != 0, ]
    } else {
      df_of_interest_stats <- df_of_interest
    }
    activity_stats$mean <- mean(df_of_interest_stats$activity)
    activity_stats$median <- median(df_of_interest_stats$activity)
    activity_stats$sd <- sd(df_of_interest_stats$activity)
    activity_stats$max <- min(df_of_interest_stats$activity)
    activity_stats$max <- max(df_of_interest_stats$activity)
    
    return(df_of_interest)
  })
  
  ## Create the output table
  output$table <- renderDataTable(df_of_interest())
  
  ## Create output plot
  output$plot <- renderPlot({
    ggplot(df_of_interest(),
           mapping = aes(x = yearmo, y = activity, fill = category_name)) +
      ggtitle(paste0(
        #"",
        paste0(input$ip_categories, collapse = ", "),
        " as of the " ,
        append_date_suffix(input$ip_day),
        " day of the month.")) +
      geom_col() +
      geom_hline(yintercept = activity_stats$mean - (1 * activity_stats$sd)) +
      geom_hline(yintercept = activity_stats$median) +
      geom_hline(yintercept = activity_stats$mean + (1 * activity_stats$sd)) +
      geom_text(aes(
        x = min(as.numeric(yearmo)),
        y = activity_stats$mean - (1 * activity_stats$sd),
        label = "-1 S.D.: ",
        vjust = -1.0
      )) +
      geom_text(aes(
        x = min(as.numeric(yearmo)),
        y = activity_stats$median,
        label = "Median: ",
        vjust = -1.0
      )) +
      geom_text(aes(
        x = min(as.numeric(yearmo)),
        y = activity_stats$mean + (1 * activity_stats$sd),
        label = "+1 S.D.: ",
        vjust = -1.0
      )) +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_fill_discrete(name = "Category") +
      scale_x_discrete(name = "Year-Month") +
      scale_y_continuous(name = "Net activity ($)",
                         labels = scales::dollar,
                         breaks = sort(
                           c(
                             seq(0,
                                 ceiling(activity_stats$max / 10) * 10,
                                 length.out = 2),
                             activity_stats$mean - (1 * activity_stats$sd),
                             activity_stats$median,
                             activity_stats$mean + (1 * activity_stats$sd)
                           )
                         ))
    
  })
  
  output$plotly <- renderPlotly({
    
    ## Formatting for text annotations for statistics
    annotations = list(xref = "yearmo", x = ~yearmo[1],  xanchor = 'right', yanchor = 'middle', 
                       font = list(family = 'Arial', size = 10,  color = 'rgba(67,67,67,1)'), showarrow = FALSE)
    
    ## Formatting for horizontal lines
    hlinefont <- list(color = 'rgba(0,0,0, 1)', width = 2)
    
    ## Create plot
    plot_ly(df_of_interest(), x=~yearmo, y=~activity, type='bar',
            name=~category_name, color=~category_name) %>%
      layout(barmode='stack') %>%
      
      ## Line and text for Mean - 1 SD
      add_segments(x = ~yearmo[1], xend = ~yearmo, showlegend=FALSE, line = hlinefont, 
                   name="Average - 1 SD", 
                   y = (activity_stats$mean - activity_stats$sd), 
                   yend = (activity_stats$mean - activity_stats$sd)) %>%
      layout(annotations = c(annotations, list(text = paste('Average - 1 SD'), 
                                               y = (activity_stats$mean - activity_stats$sd)))) %>%
      
      ## Line and text for Mean
      add_segments(x =~yearmo[1], xend = ~yearmo, showlegend=FALSE, line = hlinefont, 
                   name="Median",
                   y = activity_stats$median, 
                   yend = activity_stats$median) %>%
      layout(annotations = c(annotations, list(text = paste('Median'), y = (activity_stats$median)))) %>%
      
      ## Line and text for Mean + 1 SD
      add_segments(x = ~yearmo[1], xend = ~yearmo, showlegend=FALSE, line = hlinefont, 
                   name="Average + 1 SD", 
                   y = (activity_stats$mean + activity_stats$sd), 
                   yend = (activity_stats$mean + activity_stats$sd)) %>%
      layout(annotations = c(annotations, list(text = paste('Average + 1 SD'), 
                                               y = (activity_stats$mean + activity_stats$sd)))) %>%
      
      ## Other layout features
      layout(xaxis = list(title = "Year-month", tickangle=315), yaxis = list(title="Net spending", tickprefix="$"))
  })
}

## Shiny App
shinyApp(ui = ui, server = server)
