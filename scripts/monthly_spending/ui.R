## Title: ynabR: Track Monthly Spending App
## Author: Mathew Roy
## Comment: Displays monthly spending amounts between a start and end date,
##          for budget categories of interest and
##          compares it to historical spending as of the day of the month.
## Created on: November 5, 2018
## Updated on: January 1, 2020

## User interface
fluidPage(
  titlePanel("ynabr Spending Tracker: Month-to-day spending compared to past months"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      ## token input
      textInput(inputId = "ip_token", label = "Personal Access Token",value = ""),
      
      ## enter token
      actionButton(inputId = "ip_entertoken", "Enter"),
      
      ## budget input
      htmlOutput(outputId = "op_budgetlist"),
      
      ## enter token
      actionButton(inputId = "ip_dltransactions", "Refresh"),
      
      htmlOutput(outputId = "op_categories"),
      
      sliderInput(inputId = "ip_day", min = 1, max = 31, step = 1, #value = 31,
                  value = lubridate::day(Sys.Date()), round = TRUE, animate = TRUE, 
                  label = "As of day"),
      
      htmlOutput(outputId = "op_dateranges"),
      
      ## Exclude months with zero net activity
      checkboxInput(inputId = "ip_exclude",
                    label = "Exclude months with net activity of zero in statistics",
                    value = TRUE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotlyOutput("plotly")),
                  tabPanel("Table", dataTableOutput("table")))
    )
  )
)