library(googlesheets4)
library(dplyr)
library(magrittr)
library(quantmod)
library(TTR)
library(lubridate)
source("R/screen-pattern-match.R")

## first Monday in 2019 until 74 weeks later
mondays <- ymd("2019-01-07") + 7 * 0:74

## get stocks with sale price between 1 and 5 dollars
stocks <- TTR::stockSymbols() %>%
  as.data.frame()


## run backtest
backtest_pattern_screen <- lapply(stocks$Symbol, function(s) {
  
  cat("Running screen for stock: ", as.character(s), "\n")
  stockdata <- getSymbols(s, auto.assign = FALSE)
  
  ## loop through weeks for each stock
  lapply(mondays, function(m) {
    ## run screen
    cat("Running screen for day: ", as.character(m), "\n")
    
    ## get stock data from beginning until the monday being studied
    stockdata_hist <- stockdata[paste0("/", m)]
    
    screen_type <- c("percent", "poly", "legendre", "ns", "loess")
    lapply( screen_type, function(st) {
      cat("Running screen using type: ", st, "\n")

      screen_out <- tryCatch({
        pattern_match_screen(stockdata = stockdata_hist,
                             window_length = 60,
                             comparisons = 10,
                             method = st,  # percent, poly, legendre, ns, loess
                             degree = 5,
                             df = 5,
                             span = 0.75)
        },
        error = function(e) list(avg_percent_change = 0, proportion_up = 0))
    
        ## open price monday else return 0
        mon <- as.numeric(Op(stockdata[m]))
        op <- ifelse(length(mon) == 0, 0, mon)
      
        ## close price friday else return 0
        fri <- as.numeric(Cl(stockdata[m+4]))
        cl <- ifelse(length(fri) == 0, 0, fri)
      
        ## make output data.frame
        data.frame(buy_date = as.character(m),
                   sell_date = as.character(m+4),
                   symbol = s, 
                   monday_open = op, 
                   friday_close = cl, 
                   screen_type = st,
                   parameters = "NA", 
                   screen_avg_change = screen_out$avg_percent_change, 
                   screen_prop_up = screen_out$proportion_up)
    }) %>%
      do.call(rbind, .) %>%
      as.data.frame() ## END: screen type
  
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame() ## END: screen day
  
})  %>%
  do.call(rbind, .) %>%
  as.data.frame() ## END: stock


write.csv(backtest_pattern_screen, 'backtest-pattern-screen-1week.csv')
