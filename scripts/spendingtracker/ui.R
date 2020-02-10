## Author: Mathew Roy
## Updated on: February 10, 2020
## With help from sources acknowledged below

library(plotly)
## User interface
ui <- fluidPage(
  titlePanel("Month-to-day spending tracker app for YNAB"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
     
      ## budget input
      htmlOutput(outputId = "op_budgetlist"),
      
      ## select budget
      actionButton(inputId = "ip_dltransactions", "1. Select this budget"),
      
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
                  tabPanel("Notes & Privacy",
                           h3("Description"),
                           p("This app tracks your spending across budget categories and 
                             shows the typical net spending based on the budget, budget categories, 
                             day of the month, and time period selected. I created it for fun to gain 
                             insights on my spending habits and also to learn Shiny. I hope you find it useful.
                             This project is open-source and you can find more information about it in the 'scripts' 
                             subdirectory of my GitHub page (see below)."),
                           h3("Privacy"),
                           p("From shinyapps.io: 'shinyapps.io is secure-by-design. Each Shiny application runs 
                             in its own protected environment and access is always SSL encrypted'."),
                           p("The data is only temporarily stored on the secure shinnyapps.io servers for 
                             the length of the usage session."),
                           p("After the instance of this app is ended (closed), your data is no longer kept on the servers."),
                           p("Your data will not be transferred to any third party."),
                           h3("Disclaimer"),
                           p("This app is not sponsored, endorsed or supported by ",
                             tags$a(href="https://www.youneedabudget.com", "You Need A Budget.")),
                           h3("Author"),
                           p("Mathew Roy:",
                             tags$a(href="https://www.linkedin.com/in/matroy","[My LinkedIn Profile]"),
                             tags$a(href="https://github.com/mathewroy/ynabr","[ynabr GitHub]"))
                           )
                           )
    )
  )
)

# Source: Hadley Wickham's github-oauth example script
uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    
    url <- httr::oauth2.0_authorize_url(api, app, scope = scope)
    redirect <- sprintf("location.replace(\"%s\");", url)
    tags$script(HTML(redirect))
    
  } else {
    ui
  }
}

