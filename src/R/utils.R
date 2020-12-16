#' get_stocks
#' 
#' 
#' 
#' 
library(dplyr)
library(magrittr)
# stocks <- TTR::stockSymbols() %>%
#   as.data.frame() %>%
#   filter(LastSale > 2, LastSale < 10)



#' rescale_to_current
#' 
#' Scale the current price to a price that reflects the current price
#'
#'
#'


rescale_to_current <- function(current_price,
                               check_price) {
  
  df <- data.frame(x = as.numeric(scale(current_price)),
                   y = current_price)
  
  fit1 <- lm(y ~ x, data = df)
  newdata <- data.frame(x = as.numeric(scale(check_price)))
  out <- predict(fit1, newdata)
  out
  
}



#'
#'
#'
#'

my_ggplot <- function( plot_data, pattern = "pattern1_rescale" ) { 
  
  ggplot(plot_data, aes(date, price)) + 
    geom_point() + 
    geom_line() +
    geom_point(aes_string("date", pattern), color="red") + 
    geom_line(aes_string("date", pattern),color="red") 
  
}


#' two_week_return
#'
#' This function will take in an xts dataset and conducat a bayesian change point analysis (from the bcp package)
#' on the daily returns of the stock price.  It then returns the data with the appended posterior probabilities of a change point
#' occuring and also a list of the dates of the events where the probability is above the cutoff value given.
#'
#' @param stockdata Either time series data or numeric data. For time series, this must be an xts object or an object which is convertible to xts. For numeric data, this must be a named list or data frame, where the first element/column provides x-axis values and all subsequent elements/columns provide one or more series of y-values
#' @param start_index
#' @return a summary of the linear model returned after the selection procedure
#' @author Kirk Gosik kdgosik@gmail.com
#' @details
#' Conducts a change point analysis on the daily returns of a stock price.
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%$%`


two_week_return <- function( stockdata, start_index ) {
  if (xts::xtsible(stockdata)) {
    if (!xts::is.xts(stockdata))
      data <- xts::as.xts(stockdata)
    format <- "date"
  }
  else if (is.list(stockdata) && is.numeric(stockdata[[1]])) {
    if (is.null(names(stockdata)))
      stop("For numeric values, 'data' must be a named list or data frame")
    format <- "numeric"
  }
  else {
    stop("Unsupported type passed to argument 'data'.")
  }
  
  out <- as.numeric(Cl(stockdata)[start_index + 10]) / as.numeric(Cl(stockdata)[start_index])
  
  return(out)
  
}




#' graph_events
#'
#' This function will take in an xts dataset and conducat a bayesian change point analysis (from the bcp package)
#' on the daily returns of the stock price.  It then returns the data with the appended posterior probabilities of a change point
#' occuring and also a list of the dates of the events where the probability is above the cutoff value given.
#'
#' @param stockdata Either time series data or numeric data. For time series, this must be an xts object or an object which is convertible to xts. For numeric data, this must be a named list or data frame, where the first element/column provides x-axis values and all subsequent elements/columns provide one or more series of y-values
#' @param start_index
#' @return a summary of the linear model returned after the selection procedure
#' @author Kirk Gosik kdgosik@gmail.com
#' @details
#' Conducts a change point analysis on the daily returns of a stock price.
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%$%`

graph_events <- function( screen_list ) {
  
  data <- screen_list$data
  events <- screen_list$events
  
  dygraphs::dygraph(OHLC(data)) %>%
    dygraphs::dyCandlestick() %>%
    dygraphs::dyEvent(events)
}



graph_with_ribbon <- function( stockdata ) {
  
  stockdata$EMA50 <- EMA(Cl(stockdata), n = 50)
  stockdata$EMA200 <- EMA(Cl(stockdata), n = 200)
  
  difference <- stockdata$EMA50 - stockdata$EMA200
  decreasing <- which(difference < 0)
  increasing <- which(difference > 0)
  
  ribbonData <- rep(0, nrow(stockdata))
  ribbonData[decreasing] <- 0.5
  ribbonData[increasing] <- 1
  
  dygraph(stockdata[,c(1,2,3,4,7,8)]) %>%
    dyCandlestick() %>%
    dyRibbon(data = ribbonData, top = 1, bottom = 0.02)
  
}




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





