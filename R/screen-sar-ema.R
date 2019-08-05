#' Change Point Stock Screening
#'
#' This function will take in an xts dataset and conducat a bayesian change point analysis (from the bcp package)
#' on the daily returns of the stock price.  It then returns the data with the appended posterior probabilities of a change point
#' occuring and also a list of the dates of the events where the probability is above the cutoff value given.
#'
#' @param data Either time series data or numeric data. For time series, this must be an xts object or an object which is convertible to xts. For numeric data, this must be a named list or data frame, where the first element/column provides x-axis values and all subsequent elements/columns provide one or more series of y-values
#' @param cutoff
#' @return a summary of the linear model returned after the selection procedure
#' @author Kirk Gosik kdgosik@gmail.com
#' @details
#' Conducts a change point analysis on the daily returns of a stock price.
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%$%`

library(magrittr)
library(dplyr)
library(dygraphs)
library(quantmod)
library(bcp)
library(pracma)


## Techincal Combo: EMA, SAR and MACD #######

SARandEMA_screen <- function( stockdata ){
  
  if (xts::xtsible(stockdata)) {
    if (!xts::is.xts(stockdata))
      stockdata <- xts::as.xts(stockdata)
    format <- "date"
  }
  else if (is.list(data) && is.numeric(stockdata[[1]])) {
    if (is.null(names(data)))
      stop("For numeric values, 'data' must be a named list or data frame")
    format <- "numeric"
  }
  else {
    stop("Unsupported type passed to argument 'data'.")
  }
  
  workingdata <- cbind(quantmod::OHLC(stockdata),
                       TTR::SAR(stockdata),
                       TTR::EMA(quantmod::Cl(stockdata), n = 20),
                       TTR::MACD(quantmod::Cl(stockdata))
  ) %>% as.data.frame
  colnames(workingdata) <- c("Open", "High", "Low", "Close", "SAR", "EMA20", "MACD", "MACDsignal")
  
  workingdata <- workingdata %>%
    dplyr::mutate(Date = as.Date(index(stockdata)),
                  SARAboveCandle = as.numeric(SAR > High),
                  SARNearHigh = as.numeric(between(SAR / High, 1, 1.005)),
                  SARBelowMA = as.numeric(SAR < EMA20),
                  MACDAboveSignal = as.numeric(MACD > MACDsignal),
                  Position = as.numeric(SARAboveCandle + SARNearHigh + SARBelowMA == 3)
    ) %>%
    ts %>%
    as.xts
  
  index(workingdata) <- index(stockdata)
  
  # buy_events <- stockdata %>%
  #   dplyr::filter(Position == 1) %>%
  #   dplyr::select(Date)
  
  buy_events <- index(workingdata)[which(workingdata$Position == 1)]
  
  list(data = stockdata,
       events = buy_events)
}

