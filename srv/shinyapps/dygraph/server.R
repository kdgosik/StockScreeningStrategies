# server section
server <- function(input, output, session) {
  
  observeEvent(input$update_symbols, {
    
    stocks <- TTR::stockSymbols() %>%
      as.data.frame() %>%
      filter(LastSale > input$price[1], LastSale < input$price[2])
    
    updateSelectInput(session, "symbol",
                      label = "Select Symbol",
                      choices = stocks$Symbol
    )
  })
  
  
  
  reactive_df <- eventReactive(input$create, {
    
    df <- getSymbols(input$symbol, auto.assign = FALSE)
    df
    
  })
  
  # Show the graph
  output$plot1 <- renderDygraph({
    
    dygraph(OHLC(reactive_df()))%>% dygraphs::dyCandlestick()
    
  })
}