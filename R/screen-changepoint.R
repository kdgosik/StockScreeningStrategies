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

## Change Point ########

changepoint_screen <- function( stockdata, cutoff = 0.9 ) {
  
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
  
  bcp_post <- stockdata %>%
    dailyReturn %>%
    bcp %$%
    posterior.prob %>%
    ts %>%
    as.xts
  
  index(bcp_post) <- index(stockdata)
  bcp_events <- index(stockdata)[which(bcp_post > cutoff)]
  
  list(data = cbind(stockdata, bcp_post),
       events = bcp_events)
}
