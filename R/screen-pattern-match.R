#' Lengendre Polynomial Function
#'
#' This function is for calculating the legendre polynomial function for a given order.
#'
#' @param t Time period to use
#' @param np.order Order of the Polynomial to calculate
#' @return a summary of the linear model returned after the selection procedure
#' @param tmin
#' @param tmax
#' @author Kirk Gosik kdgosik@gmail.com
#' @details
#' Calculates the legendre polynomial for a given time period.
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%$%`

require(pracma)
require(quantmod)
require(splines)


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




#' Pattern Match Stock Screening
#'
#' This function intakes a ticker symbol and perform a pattern matching of the historical
#' trend of the data that matches closest to the current time period back to a certain window
#' length.  For example, if the window=60, it will take the last 60 days of trading and find
#' the closest 60 day time period in the historic data and use that to see what happens in the
#' future price action.  
#' 
#' @param symbol Stock symbol to use for the screen
#' @param window_length
#' @param comparisons
#' @param method
#' @param degree
#' @param df
#' @param span
#' @return a summary of the linear model returned after the selection procedure
#' @author Kirk Gosik kdgosik@gmail.com
#' @details
#' Conducts a pattern matching stock screening strategy
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%$%`


pattern_match_screen <- function( stockdata,
                                  window_length = 60,
                                  comparisons = 10,
                                  method = "legendre",
                                  degree = 5,
                                  df = 5,
                                  span = 0.75,
                                  test_points = 0 ) {
  
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
  current_y <- as.vector(scale(vec[(end - (win - 1)) : end]))
  x <- seq(1, win)
  
  ## uses current_y to make predictions using specified method
  current_y_pred <- switch(
    EXPR = method,
    percent = current_y / (dplyr::last(current_y)),
    poly = predict(lm(current_y ~ poly(x, degree))),
    legendre = predict(lm(current_y ~ Legendre(t = x, n = degree))),
    ns = predict(lm(current_y ~ ns(x, df = df))),
    loess = predict(loess(current_y ~ x, span = span))
    )
  
  ## Calculate RSE from current vs predicted
  norm_val <- NULL
  for( i in 1 : (end - (2 * win)) ) {
    check_y <- as.vector(scale(vec[i : (i + (win - 1))]))
    check_y_pred <- switch(method,
                           percent = check_y / (dplyr::last(check_y)),
                           poly = predict(lm(check_y ~ poly(x, degree))),
                           legendre = predict(lm(check_y ~ Legendre(t = x, n = degree))),
                           ns = predict(lm(check_y ~ ns(x, df = df))),
                           loess = predict(loess(check_y ~ x, span = span)))
    
    norm_val[i] <- sum((current_y_pred - check_y_pred)^2)^(1/2)
  }
  
  ## get indexes of the top n comparisons 
  idx <- which(norm_val <= sort(norm_val)[comparisons])
  
  ## returns data and summarys statistics on the screening procedure
  list(data = stockdata,
       percent_change = vec[(idx + (win + 20))] / vec[idx],
       index = idx,
       avg_percent_change = harmmean(vec[(idx + (win + 20))] / vec[idx]),
       proportion_up = mean(as.numeric(vec[(idx + (win + 20))] / vec[idx] > 1)))
  
}
