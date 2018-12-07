library(shiny)
library(tidyverse)
ui <- fluidPage(
  selectInput(inputId = "ip_categories",
              choices = sort(unique(df_transactions$category_name)),
              label = "Category"),
  plotOutput("plot_spending")
)

server <- function(input, output) {
  
  df_of_interest <- reactive({
    df_transactions %>% filter(category_name == input$ip_categories) %>% 
      group_by(category_name, yearmo) %>%
      summarize(activity = -1 * sum(amount) / 1E3)

  })
  
  ## Create plot
  output$plot_spending <- renderPlot({
    ggplot(df_of_interest(), mapping = aes(x = yearmo, y = activity, fill = category_name)) + 
      geom_col()
  })

}

shinyApp(ui = ui, server = server)