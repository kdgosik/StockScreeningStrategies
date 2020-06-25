library(quantmod)
library(TTR)
library(magrittr)
library(dygraphs)
library(dplyr)
library(shiny)


# ui section
ui = fluidPage(

  #  Title
  titlePanel("Dygraph Stocks"),

  # Sidebar with slider and controls for animation
  sidebarLayout(

    # sidebar with slider
    sidebarPanel(

      sliderInput(inputId = "price", label = "Price Range", min = 1, max = 500, value = c(1,50), step = 0.1),

      actionButton(inputId = "update_symbols", label = "Update Symbols"),

      selectInput(inputId = "symbol", label = "Select Symbol", choices = ""),

      actionButton(inputId = "create", "Create Plot")
    ),

    # Show the graph
    mainPanel(
      dygraphOutput(outputId = "plot1")
    )
  )
)



# server section
server = function(input, output, session) {

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

# run the app
runApp(list(ui = ui, server = server), launch.browser = TRUE)
