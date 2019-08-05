#' Natural Splines Stock Screening
#'
#' This function will take in an xts dataset and conducat a bayesian change point analysis (from the bcp package)
#' on the daily returns of the stock price.  It then returns the data with the appended posterior probabilities of a change point
#' occuring and also a list of the dates of the events where the probability is above the cutoff value given.
#'
#' @param stockdata Either time series data or numeric data. For time series, this must be an xts object or an object which is convertible to xts. For numeric data, this must be a named list or data frame, where the first element/column provides x-axis values and all subsequent elements/columns provide one or more series of y-values
#' @param window_length
#' @param comparisons
#' @param method
#' @param degree
#' @param df
#' @param span
#' @return a summary of the linear model returned after the selection procedure
#' @author Kirk Gosik kdgosik@gmail.com
#' @details
#' Conducts a change point analysis on the daily returns of a stock price.
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%$%`



sma_screen <- function( stockdata, short = 50, long = 200 ) {
  
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
  
  
  short_sma <- SMA( Cl(stockdata), n = short )
  long_sma <- SMA( Cl(stockdata), n = long )
  run_corr <- runCor(as.numeric(index(stockdata)), Cl(stockdata))
  
  return(list(
    value = short_sma - long_sma,
    buy = short_sma - long_sma > 0,
    cor = run_corr
  ))
  
}


perform_sma_screen <- function( symbol ) {
  # df <- getSymbols(symbol, auto.assign = FALSE)
  # df <- loadSymbols(symbol, auto.assign = FALSE)
  df <- eval(parse(text = symbol))
  
  screen_output <- sma_screen(df)
  
  out <- screen_output$buy[NROW(screen_output$buy),] && 
    (!{screen_output$buy[NROW(screen_output$buy)-1,]}) &&
    screen_output$run_corr > 0
  names(out) <- symbol
  out
  
}




# stockdata_dt <- as.data.table(HMY)
back_test_sma <- function( stockdata, short = 50, long = 200, cor_period = 20 ){
  
  stockdata_dt <- as.data.table(stockdata)
  
  stockdata_dt[, runcorr := runCor(.I, .SD, n = cor_period, use = "complete.obs"), .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  
  stockdata_dt[, SMAShort := lapply(.SD, SMA, n = short), .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, SMALong := lapply(.SD, SMA, n = long), .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, SMADIFF := SMAShort - SMALong]
  stockdata_dt[, SMAShortaboveLong := SMADIFF > 0] # the short time period MA is above the longer time period MA
  
  stockdata_dt[, SMAShortaboveLonglag := lag(SMAShortaboveLong)] # use to make sure we get the first instance 
  stockdata_dt[, SMAShortaboveLonglead := lead(SMAShortaboveLong)]
  stockdata_dt[, buy := SMAShortaboveLong & !{SMAShortaboveLonglag} & runcorr > 0]
  
  stockdata_dt[, OneWeek := lapply(.SD, lead, 5), .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, TwoWeek := lapply(.SD, lead, 10), .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, ThreeWeek := lapply(.SD, lead, 15), .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, FourWeek := lapply(.SD, lead, 20), .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  
  stockdata_dt[, OneWeekChange := OneWeek / .SD, .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, TwoWeekChange := TwoWeek / .SD, .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, ThreeWeekChange := ThreeWeek / .SD, .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, FourWeekChange := FourWeek / .SD, .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]

  stockdata_dt[, OneWeekMin := lapply(.SD, runMin, n = 5), .SDcols = grep("Low", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, OneWeekMin := lead(OneWeekMin, n = 5)]
  stockdata_dt[, OneWeekMaxLoss := OneWeekMin / .SD, .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]  
  
  stockdata_dt[, TwoWeekMin := lapply(.SD, runMin, n = 10), .SDcols = grep("Low", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, TwoWeekMin := lead(TwoWeekMin, n = 10)]
  stockdata_dt[, TwoWeekMaxLoss := TwoWeekMin / .SD, .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  
  stockdata_dt[, ThreeWeekMin := lapply(.SD, runMin, n = 15), .SDcols = grep("Low", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, ThreeWeekMin := lead(ThreeWeekMin, n = 15)]
  stockdata_dt[, ThreeWeekMaxLoss := ThreeWeekMin / .SD, .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]
  
  stockdata_dt[, FourWeekMin := lapply(.SD, runMin, n = 20), .SDcols = grep("Low", colnames(stockdata_dt), value = TRUE)]
  stockdata_dt[, FourWeekMin := lead(FourWeekMin, n = 20)]
  stockdata_dt[, FourWeekMaxLoss := FourWeekMin / .SD, .SDcols = grep("Close", colnames(stockdata_dt), value = TRUE)]

  stockdata_dt[, percent_change := TwoWeekChange]
  stockdata_dt[TwoWeekMaxLoss < 0.95, percent_change := 0.95]

  # vec[vec > 1.2] <- 1.2
  list(data = stockdata_dt,
       percent_change = stockdata_dt[buy == "TRUE", percent_change],
       cum_percent_change = Reduce("*", stockdata_dt[buy == "TRUE", percent_change]),
       buy = last(stockdata_dt$buy)
  )

}


