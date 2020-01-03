library(plotly)
## Title: ynabR: Track Monthly Spending App
## Author: Mathew Roy
## Comment: Displays monthly spending amounts between a start and end date,
##          for budget categories of interest and
##          compares it to historical spending as of the day of the month.
## Created on: November 5, 2018
## Updated on: January 2, 2020

## User interface
fluidPage(
  titlePanel("Month-to-day spending tracker app for YNAB"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      ## token input
      textInput(inputId = "ip_token", label = "YNAB Personal Access Token",
                value = ""),
      
      ## enter token
      actionButton(inputId = "ip_entertoken", "1. Submit token"),
      
      ## budget input
      htmlOutput(outputId = "op_budgetlist"),
      
      ## select budget
      actionButton(inputId = "ip_dltransactions", "2. Choose this budget"),
      
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
                  tabPanel("Table", dataTableOutput("table")),
                  tabPanel("Notes",
                           h3("Description"),
                           p("This app tracks your spending across budget categories and 
                             shows the typical net spending based on the budget, budget categories, 
                             day of the month, and time period selected. I created it for fun to gain 
                             insights on my spending habits and also to learn Shiny. I hope you find it useful.
                             This project is open-source and you can find more information about it in the 'scripts' 
                             subdirectory of my GitHub page (see below)."),
                           p("To obtain your personal access token, Sign in to YNAB > My Account > ",
                             tags$a(href="https://app.youneedabudget.com/settings/developer", "Developer Settings."),
                             "Click on New Token, enter your password, and Click Generate"),
                           h3("Privacy"),
                           p("From shinyapps.io: 'shinyapps.io is secure-by-design. Each Shiny application runs 
                             in its own protected environment and access is always SSL encrypted'.
                             To visualize your data, it is temporarily stored on the secure shinnyapps.io servers for 
                             the length of the usage session. 
                             Only you, the user, have access to your own token and associated data. 
                             After the instance of this app is ended (closed), your data is no longer kept on the servers."),
                           h3("Author"),
                           p("Mathew Roy:",
                             tags$a(href="https://www.linkedin.com/in/matroy","[My LinkedIn Profile]"),
                             tags$a(href="https://github.com/mathewroy/ynabr","[ynabr GitHub]"))
                           )
                  )
    )
  )
)