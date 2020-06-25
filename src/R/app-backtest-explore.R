library(shiny)
library(tidyverse)
library(plotly)


## helpers.R #########

to_avg <- function(x) {
  sapply(x, function(y) mean(as.numeric(unlist(strsplit(y, ";")))), USE.NAMES = FALSE)
}


to_prop <- function(x) {
  sapply(x, function(y) mean(as.numeric(unlist(strsplit(y, ";"))) > 1), USE.NAMES = FALSE)
}


unique_patterns <- function(x) {
  
  sapply(x, function(y) {
    
    idx <- as.numeric(unlist(strsplit(y, ";")))
    sum(idx[2:10] - idx[1:9] > 1) + 1
    
  }, USE.NAMES = FALSE)

}

## global.R #########

data_path <- "~/Documents/projects/StockScreeningStrategies/data/"
# backtest <- read_csv(paste0(data_path, "backtest-screen-pattern-match-old.csv")) %>%
#   filter(symbol %in% stocks$Symbol[1:1000]) %>%
backtest <- read_csv(paste0(data_path, "backtest-screen-pattern-match.csv")) %>%
  filter(monday_open > 0, friday_close > 0) %>%
  mutate(percent_change = friday_close / monday_open)


## ui.R ############

ui <- fluidPage(

    sidebarPanel(
      selectInput(inputId = "metric",
                  label = "Select Screen Metric",
                  choices = c("percent_change_window_hold", "percent_change_window_holdlook",
                              "percent_change_present_hold", "percent_change_present_holdlook")),
      
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
                   step = 1),
      
      actionButton(inputId = "update",
                   label = "Update Data")
      
      ),
    mainPanel(
      plotlyOutput(outputId = "plot1"),
      dataTableOutput(outputId = "return")
    )
    
)


## server.R #############

server <- function(input, output, session) {
  
  df_reactive <- eventReactive(input$update, {
    
    backtest %>%
      rename(screen_change = ends_with(match = input$metric)) %>% ## screen_change
      mutate(screen_avg_change = to_avg( screen_change ),  ## percent_change_present_hold
             screen_prop_up = to_prop( screen_change )) %>%
      select(buy_date, sell_date, symbol, monday_open, friday_close, screen_type, 
             screen_avg_change, screen_prop_up, percent_change) %>%
      filter(screen_prop_up  == 1,
             screen_type == input$screen_type, 
             screen_avg_change > input$screen_avg_change,
             monday_open < input$price_cutoff) %>%
      group_by(buy_date) %>% 
      arrange(-screen_avg_change) %>% 
      # slice_head(n = input$topn) %>%
      top_n(n = input$topn) %>%
      group_by(buy_date) %>% 
      summarise(gross = sum(percent_change), total = n(), return = gross/total) %>% 
      filter(return > 0) %>% 
      ungroup %>%
      mutate(cumprod = round(cumprod(return), 4))
    
  })
  
  
  ## rendering Data Table
  output$return <- renderDataTable({
    
    df_reactive()
    
  }, options = list(pageLength = 10))
  
  
  ## rendering Plotly
  output$plot1 <- renderPlotly({
    
    ggplotly(
      ggplot(df_reactive(), aes(x = buy_date, y = cumprod)) + 
        geom_col() +
        scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d")
    )
    
  })
  
}


## run shiny app
runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
