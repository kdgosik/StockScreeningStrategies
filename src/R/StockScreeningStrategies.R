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


graph_events <- function( screen_list ) {

  data <- screen_list$data
  events <- screen_list$events

  dygraphs::dygraph(OHLC(data)) %>%
    dygraphs::dyCandlestick() %>%
    dygraphs::dyEvent(events)
}


## Techincal Combo: EMA, SAR and MACD #######

#' EMA, SAR and MACD Stock Screening
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


## Legendre Polynomial Function ################

#' Legendre Polynomial Function
#'
#'
#' @param t Either time series data or numeric data. For time series, this must be an xts object or an object which is convertible to xts. For numeric data, this must be a named list or data frame, where the first element/column provides x-axis values and all subsequent elements/columns provide one or more series of y-values
#' @param np.order
#' @param tmin
#' @param  tmax
#' @return legendre polynomial matrix
#' @author Kirk Gosik kdgosik@gmail.com
#' @details
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%$%`
#' 
require(pracma)
require(quantmod)
require(splines)
# rm(list = ls()); gc(reset = TRUE)
getSymbols("NVDA")


Legendre<-function( t, np.order=1,tmin=NULL, tmax=NULL ) {
  u <- -1
  v <- 1
  if (is.null(tmin)) tmin<-min(t)
  if (is.null(tmax)) tmax<-max(t)
  nt <- length(t)
  ti    <- u + ((v-u)*(t-tmin))/(tmax - tmin)
  np.order.mat <- matrix(rep(0,nt*np.order),nrow=nt)
  if(np.order >=1)
    np.order.mat[,1] <- rep(1,nt)
  if (np.order>=2)
    np.order.mat[,2] <- ti
  if (np.order>=3)
    np.order.mat[,3] <- 0.5*(3*ti*ti-1)
  if (np.order>=4)
    np.order.mat[,4] <- 0.5*(5*ti^3-3*ti)
  if (np.order>=5)
    np.order.mat[,5] <- 0.125*(35*ti^4-30*ti^2+3)
  if (np.order>=6)
    np.order.mat[,6] <- 0.125*(63*ti^5-70*ti^3+15*ti)
  if (np.order>=7)
    np.order.mat[,7] <- (1/16)*(231*ti^6-315*ti^4+105*ti^2-5)
  if (np.order>=8)
    np.order.mat[,8] <- (1/16)*(429*ti^7-693*ti^5+315*ti^3-35*ti)
  if (np.order>=9)
    np.order.mat[,9] <- (1/128)*(6435*ti^8-12012*ti^6+6930*ti^4-1260*ti^2+35)
  if (np.order>=10)
    np.order.mat[,10] <- (1/128)*(12155*ti^9-25740*ti^7+18018*ti^5-4620*ti^3+315*ti)
  if (np.order>=11)
    np.order.mat[,11] <- (1/256)*(46189*ti^10-109395*ti^8+90090*ti^6-30030*ti^4+3465*ti^2-63)
  return(np.order.mat)
}





#' Stock Screening
#'
#' @param stockdata Either time series data or numeric data. For time series, this must be an xts object or an object which is convertible to xts. For numeric data, this must be a named list or data frame, where the first element/column provides x-axis values and all subsequent elements/columns provide one or more series of y-values
#' @param window_length
#' @param look_ahead_window number of days to look into the future after a pattern to calculate the return beyond the holding days
#' @param hold_days how long you plan on holding a stock for before selling
#' @param comparisons the number of comparison patterns to calculate
#' @param method the screenign method to use.  Either percent, poly, legendre, ns, loess
#' @param degree the degree of the poly or legendre calculation
#' @param df the degrees of freedom to use in the ns 
#' @param span the span of the loess calculation
#' 
#' @return a list of summary stats and the data
#' @author Kirk Gosik kdgosik@gmail.com
#' @details
#' Conducts a change point analysis on the daily returns of a stock price.
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%$%`
#' 
require(pracma)
require(quantmod)
require(splines)
source("/home/kirk/Documents/projects/StockScreeningStrategies/src/R/utils.R")

this_symbol <- "HMY"
this_date <- "2020-07-06"
stockdata <- getSymbols(this_symbol, auto.assign = FALSE)[paste0("/", this_date)]
window_length = 60
look_ahead_window = 60
hold_days = 5
comparisons = 10
method = "ns"
degree = 5
df = 5
span = 0.75

  
  # ## using quantmod API to get the stock data
  # stockdata <- getSymbols(symbol, auto.assign = FALSE)[paste0("/", Sys.Date() - test_points)]
  # 
  ## check to make sure data is in proper format
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
  
  ## calculates the average of the high and low price for the day
  vec <- as.numeric((Hi(stockdata) + Lo(stockdata))/2)
  
  end <- length(vec)
  win <- window_length
  
  ## current scaled price values upto the beginning of the windowed period
  current_y <- vec[(end - (win - 1)) : end]
  x <- seq(1, win)
  
  ## uses current_y to make predictions using specified method
  current_y_pred <- switch(
    EXPR = method,
    percent = current_y / (dplyr::first(current_y)), ## price change since beginning of pattern
    poly = predict(lm(current_y ~ poly(x, degree))),
    legendre = predict(lm(current_y ~ Legendre(t = x, n = degree))),
    ns = predict(lm(current_y ~ ns(x, df = df))),
    loess = predict(loess(current_y ~ x, span = span))
  )
  
  ## Calculate RSE from current vs predicted
  ## norm_val[i] is the RSE value for the index of the first day of the pattern
  norm_val <- NULL
  for( i in 1 : (end - (2 * win)) ) {
    check_y <- rescale_to_current(current_price = current_y, check_price = vec[i : (i + (win - 1))])
    check_y_pred <- switch(method,
                           percent = check_y / (dplyr::first(check_y)),  ## price change since beginning of pattern
                           poly = predict(lm(check_y ~ poly(x, degree))),
                           legendre = predict(lm(check_y ~ Legendre(t = x, n = degree))),
                           ns = predict(lm(check_y ~ ns(x, df = df))),
                           loess = predict(loess(check_y ~ x, span = span)))
    
    norm_val[i] <- sum((current_y_pred - check_y_pred)^2)^(1/2)
  }
  
  ## get indexes of the top n comparisons 
  idx <- which(norm_val <= sort(norm_val)[comparisons])
  
  ## returns data and summarys statistics on the screening procedure
  ## (idx + win - 1) - the index of the last day in the pattern
  ## (look_ahead_window + hold_days - 1)
  
  ## percent change from beginning of pattern to the hold days prediction
  percent_change_window_hold <- round(vec[(idx + (win - 1 + hold_days))] / vec[idx], 3)
  
  ## percent change from beginning of pattern to the hold days + a look ahead prediction
  percent_change_window_holdlook <- round(vec[((idx + win -1) + (look_ahead_window + hold_days))] / vec[idx], 3)
  
  ## percent change from end of the pattern to the hold days prediction
  percent_change_present_hold <- round(vec[((idx + win - 1) + (hold_days))] / vec[(idx + win)], 3)
  
  ## percent change from end of the pattern to the hold days + a look ahead prediction
  percent_change_present_holdlook <- round(vec[((idx + win -1) + (look_ahead_window + hold_days))] / vec[(idx + win)], 3)
  
  list(data = stockdata,
       index = idx,
       norm_values = round(norm_val[idx], 3),
       percent_change_window_hold = percent_change_window_hold,
       percent_change_window_holdlook = percent_change_window_holdlook,
       percent_change_present_hold = percent_change_present_hold,
       percent_change_present_holdlook = percent_change_present_holdlook
       )

  
  
## Ichimoku's Cloud ################################



## 2618 Strategy ###################################
