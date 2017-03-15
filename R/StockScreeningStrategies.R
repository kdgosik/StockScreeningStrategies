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


## Change Point ########

changepoint_screen <- function( data, cutoff = 0.9 ) {

  if (xts::xtsible(data)) {
    if (!xts::is.xts(data))
      data <- xts::as.xts(data)
    format <- "date"
  }
  else if (is.list(data) && is.numeric(data[[1]])) {
    if (is.null(names(data)))
      stop("For numeric values, 'data' must be a named list or data frame")
    format <- "numeric"
  }
  else {
    stop("Unsupported type passed to argument 'data'.")
  }

  bcp_post <- data %>%
    quantmod::dailyReturn %>%
    bcp::bcp %$%
    posterior.prob %>%
    ts %>%
    xts::as.xts

  index(bcp_post) <- index(data)
  bcp_events <- index(data)[which(bcp_post > cutoff)]

  list(data = cbind(data, bcp_post),
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

SARandEMA_screen <- function( data ){

  if (xts::xtsible(data)) {
    if (!xts::is.xts(data))
      data <- xts::as.xts(data)
    format <- "date"
  }
  else if (is.list(data) && is.numeric(data[[1]])) {
    if (is.null(names(data)))
      stop("For numeric values, 'data' must be a named list or data frame")
    format <- "numeric"
  }
  else {
    stop("Unsupported type passed to argument 'data'.")
  }

  data <- cbind(quantmod::OHLC(data),
                TTR::SAR(data),
                TTR::EMA(quantmod::Cl(data), n = 20),
                TTR::MACD(quantmod::Cl(data))
  ) %>% as.data.frame
  colnames(data) <- c("Open", "High", "Low", "Close", "SAR", "EMA20", "MACD", "MACDsignal")

  data <- data %>%
    dplyr::mutate(Date = as.Date(rownames(data)),
           SARAboveCandle = as.numeric(SAR > High),
           SARNearHigh = as.numeric(between(SAR / High, 1, 1.005)),
           SARBelowMA = as.numeric(SAR < EMA20),
           MACDAboveSignal = as.numeric(MACD > MACDsignal),
           Position = as.numeric(SARAboveCandle + SARNearHigh + SARBelowMA == 3)
    )

  buy_events <- data %>%
    dplyr::filter(Position == 1) %>%
    dplyr::select(Date)

  list(data = data,
       events = buy_events)
}


## Natural Splines Pattern Matching ################
require(pracma)
require(quantmod)
require(splines)
rm(list = ls()); gc(reset = TRUE)
getSymbols("NVDA")


    # pracma's polyfit

norm_func_search <- function( data, window_length = 30, comparisons = 10, degree = 5 ) {

  if (xts::xtsible(data)) {
    if (!xts::is.xts(data))
      data <- xts::as.xts(data)
    format <- "date"
  }
  else if (is.list(data) && is.numeric(data[[1]])) {
    if (is.null(names(data)))
      stop("For numeric values, 'data' must be a named list or data frame")
    format <- "numeric"
  }
  else {
    stop("Unsupported type passed to argument 'data'.")
  }

  data <- as.numeric((Hi(data) + Lo(data))/2)

  end <- length(data)
  win <- window_length

  current_poly <- polyfit(1 : win, as.vector(scale(data[(end - (win - 1)) : end])), n = degree)
  current_poly_func <- function(x) polyval(current_poly, x)

  norm_val <- NULL
  for( i in 1 : (end - (2 * win)) ) {
    check_poly <- polyfit(1 : win, as.vector(scale(data[i : (i + (win - 1))])), n = degree)
    check_poly_func <- function(x) polyval(check_poly, x)

    norm_val[i] <- fnorm(current_poly_func, check_poly_func, 1, win, p = Inf)
  }
  idx <- which(norm_val <= sort(norm_val)[comparisons])
  list(data[(idx + (win + 20))] / data[idx],
       idx,
       harmmean(data[(idx + (win + 20))] / data[idx]),
       mean(as.numeric(data[(idx + (win + 20))] / data[idx] > 1)))
}




  # splines's ns (natural splines) and loess smoothing

NaturalSplines_screen <- function( data, window_length = 60, comparisons = 10, degree = 5 ) {

  if (xts::xtsible(data)) {
    if (!xts::is.xts(data))
      data <- xts::as.xts(data)
    format <- "date"
  }
  else if (is.list(data) && is.numeric(data[[1]])) {
    if (is.null(names(data)))
      stop("For numeric values, 'data' must be a named list or data frame")
    format <- "numeric"
  }
  else {
    stop("Unsupported type passed to argument 'data'.")
  }

  data <- as.numeric((Hi(data) + Lo(data))/2)

  end <- length(data)
  win <- window_length

  current_y <- as.vector(scale(data[(end - (win - 1)) : end]))
  x <- seq(1, win)
  current_y_pred <- predict(lm(current_y ~ ns(x, df = degree)))

  norm_val <- NULL
  for( i in 1 : (end - (2 * win)) ) {
    check_y <- as.vector(scale(data[i : (i + (win - 1))]))
    check_y_pred <- predict(lm(check_y ~ ns(x, df = degree)))

    norm_val[i] <- Norm(current_y_pred - check_y_pred, p = Inf)
  }
  idx <- which(norm_val <= sort(norm_val)[comparisons])
  list(data[(idx + (win + 20))] / data[idx],
       idx,
       harmmean(data[(idx + (win + 20))] / data[idx]),
       mean(as.numeric(data[(idx + (win + 20))] / data[idx] > 1)))
}


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



PatternMatch_screen <- function( data,
                                 window_length = 60,
                                 comparisons = 10,
                                 method = "legendre",
                                 degree = 5,
                                 df = 5,
                                 span = 0.75 ) {

  if (xts::xtsible(data)) {
    if (!xts::is.xts(data))
      data <- xts::as.xts(data)
    format <- "date"
  }
  else if (is.list(data) && is.numeric(data[[1]])) {
    if (is.null(names(data)))
      stop("For numeric values, 'data' must be a named list or data frame")
    format <- "numeric"
  }
  else {
    stop("Unsupported type passed to argument 'data'.")
  }

  data <- as.numeric((Hi(data) + Lo(data))/2)

  end <- length(data)
  win <- window_length

  current_y <- as.vector(scale(data[(end - (win - 1)) : end]))
  x <- seq(1, win)

  current_y_pred <- switch(method,
                           percent = sapply(current_y, function(price) price/(dplyr::last(price))),
                           poly = predict(lm(current_y ~ poly(x, degree))),
                           legendre = predict(lm(current_y ~ Legendre(x, degree))),
                           ns = predict(lm(current_y ~ ns(x, df = df))),
                           loess = predict(loess(current_y ~ x, span = span)))

  norm_val <- NULL
  for( i in 1 : (end - (2 * win)) ) {
    check_y <- as.vector(scale(data[i : (i + (win - 1))]))
    check_y_pred <- switch(method,
                           percent = sapply(check_y, function(price) price/(dplyr::last(price))),
                           poly = predict(lm(check_y ~ poly(x, degree))),
                           legendre = predict(lm(check_y ~ Legendre(x, degree))),
                           ns = predict(lm(check_y ~ ns(x, df = df))),
                           loess = predict(loess(check_y ~ x, span = span)))

    norm_val[i] <- sum((current_y_pred - check_y_pred)^2)^(1/2)
  }
  idx <- which(norm_val <= sort(norm_val)[comparisons])
  list(data[(idx + (win + 20))] / data[idx],
       idx,
       harmmean(data[(idx + (win + 20))] / data[idx]),
       mean(as.numeric(data[(idx + (win + 20))] / data[idx] > 1)))
}


library(ggplot2)
library(plotly)

end <- nrow(NVDA)
win <- 60
plot_data <- data.frame(Date = c(index(NVDA)[(end - (win - 1)) : end], seq(index(NVDA)[end],index(NVDA)[end]+20, by = 1)),
                        Price = c(as.numeric(Cl(NVDA)[(end - (win - 1)) : end]), rep(NA, 21)),
                        Pattern1 = as.numeric(Cl(NVDA)[543:623]),
                        Pattern2 = as.numeric(Cl(NVDA)[776:856]),
                        Pattern3 = as.numeric(Cl(NVDA)[1060:1140]),
                        Pattern4 = as.numeric(Cl(NVDA)[1200:1280]),
                        Pattern5 = as.numeric(Cl(NVDA)[1518:1598]))

plot_data <- plot_data %>%
  mutate(Price = scale(Price),
         Pattern1 = scale(Pattern1),
         Pattern2 = scale(Pattern2),
         Pattern3 = scale(Pattern3),
         Pattern4 = scale(Pattern4),
         Pattern5 = scale(Pattern5))


ggplot(plot_data, aes(x = Date, y = Price)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  geom_smooth(aes(x = Date, y = Pattern1), color = "darkgreen", span = 0.5) +
  geom_smooth(aes(x = Date, y = Pattern2), color = "darkgreen", span = 0.5) +
  geom_smooth(aes(x = Date, y = Pattern3), color = "darkgreen", span = 0.5) +
  geom_smooth(aes(x = Date, y = Pattern4), color = "darkgreen", span = 0.5) +
  geom_smooth(aes(x = Date, y = Pattern5), color = "darkgreen", span = 0.5)


## Ichimoku's Cloud ################################



## 2618 Strategy ###################################
