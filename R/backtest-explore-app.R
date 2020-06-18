library(shiny)
library(tidyverse)
library(plotly)

data_path <- "~/Documents/projects/StockScreeningStrategies/data/"
backtest <- read_csv(paste0(data_path, "backtest-screen-pattern-match.csv")) %>%
  filter(monday_open > 0) %>%
  mutate(percent_change = friday_close / monday_open)


ui <- fluidPage(

    sidebarPanel(
      selectInput(inputId = "screen_type", 
                  label = "Select Screening Procedure", 
                  choices = unique(backtest$screen_type)),
      
      sliderInput(inputId = "screen_avg_change", 
                  label = "Screening Average Change Cutoff", 
                  min = 1, 
                  max = 2, 
                  value = 1,
                  step = 0.05),
      
      numericInput(inputId = "price_cutoff", 
                  label = "Stock Price Cutoff", 
                  min = 1, 
                  max = 1000, 
                  value = 2,
                  step = 0.5),
      
      numericInput(inputId = "topn",
                   label = "Top N Stocks",
                   min = 1,
                   max = 50,
                   value = 10,
                   step = 1)
      
      ),
    mainPanel(
      plotlyOutput(outputId = "plot1"),
      dataTableOutput(outputId = "return")
    )
    
)




server <- function(input, output, session) {
  
  df_reactive <- reactive({
    
    backtest %>% 
      filter(screen_type == input$screen_type, 
             screen_avg_change > input$screen_avg_change,
             monday_open < input$price_cutoff) %>%
      group_by(buy_date) %>% 
      arrange(-screen_avg_change) %>% 
      top_n(n = input$topn) %>% 
      group_by(buy_date) %>% 
      summarise(gross = sum(percent_change), total = n(), return = gross/total) %>% 
      filter(return > 0) %>% 
      ungroup %>%
      mutate(cumprod = round(cumprod(return), 4))
    
  })
  
  
  output$return <- renderDataTable({
    
    df_reactive()
    
  }, options = list(pageLength = 10))
  
  output$plot1 <- renderPlotly({
    
    ggplotly(
      ggplot(df_reactive(), aes(x = buy_date, y = cumprod)) + 
        geom_col()
    )
    
  })
  
}



runApp(shinyApp(ui = ui, server = server))
