library(googlesheets4)
library(tidyverse)
library(magrittr)
library(quantmod)
library(TTR)
library(lubridate)
library(foreach)
source("src/R/screen-pattern-match.R")


if( !{file.exists("data/backtest-screen-pattern-match.csv")} ) {
  write_csv(x = data.frame("buy_date", "sell_date","symbol", "monday_open", "friday_close", "screen_type",
                           "parameters", "index", "norm_values", "percent_change_window_hold", "percent_change_window_holdlook",
                           "percent_change_present_hold", "percent_change_present_holdlook"),
            path = "data/backtest-screen-pattern-match.csv",
            col_names = FALSE)
}

## first Monday in 2019 until 77 weeks later
mondays <- ymd("2019-01-07") + 7 * 0:76

## get stocks
# source("src/R/google-finance-dump.R")
stocks <- read_csv("data/google-finance-dump.csv") %>% 
  filter(str_ends(symbol, "-[A-Z]{1,3}", negate = TRUE), open > 0.5)
stock_symbols <- unique(stocks$symbol)


# Get all arguments
args <- commandArgs(trailingOnly = TRUE)

cat("Running chunk: ", args[1], "\n")
chunk <- as.numeric(args[1])
chunk_size <- 1
hold_days <- as.numeric(args[2])
look_ahead_window <- as.numeric(args[3])

cat(paste0("## chunk:", chunk, ";chunk_size:", chunk_size, ";hold_days:", hold_days, ";look_ahead_window:", look_ahead_window), "\n")

## run backtest
i <- 1
backtest_pattern_screen <- lapply(mondays[(1 + chunk_size*(chunk-1)) : (chunk_size*(chunk))], function(m) { 
  
  ## run screen
  cat("Running screen for day: ", as.character(m), "\n")
  
  start_time <- Sys.time()

    ## loop through weeks for each stock
    # clust <- parallel::makeCluster(4)
    # doParallel::registerDoParallel(clust)
    # foreach_packages <- c("quantmod", "TTR", "splines", "pracma", "magrittr", "dplyr")
    # foreach_exports <- c("stocks", "Legendre", "screen_pattern_match")
    # symbols_out <- foreach(s = stock_symbols,
    #                               .packages = foreach_packages,
    #                               .export = foreach_exports) %dopar% {
    symbols_out <- lapply(stock_symbols, function(s) {
      
      cat("Running screen for stock: ", as.character(s), " at time: ", as.character(start_time), "\n")
      # stockdata <- getSymbols(s, auto.assign = FALSE)
      stockdata <- stocks %>%
        filter(symbol == s) %>%
        select(date, open, high, low, close) %>%
        as.xts(x = .[,c("open", "high", "low", "close")], order.by =.$date)
      
      ## get stock data from beginning until the monday being studied
      stockdata_hist <- stockdata[paste0("/", m)]
    
      screen_type <- c("percent", "poly", "legendre", "ns", "loess") # [c(1,4)]
      lapply( screen_type, function(st) {
        cat("Running screen using type: ", st, "\n")

        screen_out <- tryCatch({
          
          window_length <- 60
          comparisons <- 10
          degree <- 5
          degfree <- 5
          span <- 0.75
          parameters <- paste0("window_length:", window_length,
                               ";look_ahead_window:", look_ahead_window,
                               ";hold_days:", hold_days,
                               ";comparisons:", comparisons, 
                               ";degree:", degree,
                               ";df:", degfree,
                               ";span:", span,
                               ";chunk:", chunk,
                               ";chunk_size:", chunk_size)
          
          screen_pattern_match(stockdata = stockdata_hist,
                               window_length = window_length,
                               look_ahead_window = look_ahead_window,
                               hold_days = hold_days,
                               comparisons = comparisons,
                               method = st,  # percent, poly, legendre, ns, loess
                               degree = degree,
                               df = degfree,
                               span = span)
          },
          error = function(e) list(
            index = NA,
            norm_values = NA,
            percent_change_window_hold = NA, 
            percent_change_window_holdlook = NA,
            percent_change_present_hold = NA,
            percent_change_present_holdlook = NA
          ))
    
          ## open price monday else return 0
          mon <- as.numeric(Op(stockdata[m]))
          op <- ifelse(length(mon) == 0, 0, mon)
      
          ## close price friday else return 0
          fri <- as.numeric(Cl(stockdata[m + (hold_days / 5) * 7 - 3]))
          cl <- ifelse(length(fri) == 0, 0, fri)
      
          ## make output data.frame
          data.frame(buy_date = as.character(m),
                     sell_date = as.character(m + (hold_days / 5) * 7 - 3),
                     symbol = s, 
                     monday_open = op, 
                     friday_close = cl, 
                     screen_type = st,
                     parameters = parameters,
                     index = paste0(screen_out$index, collapse = ";"),
                     norm_values = paste0(screen_out$norm_values, collapse = ";"),
                     percent_change_window_hold = paste0(screen_out$percent_change_window_hold, collapse = ";"),
                     percent_change_window_holdlook = paste0(screen_out$percent_change_window_holdlook, collapse = ";"),
                     percent_change_present_hold = paste0(screen_out$percent_change_present_hold, collapse = ";"),
                     percent_change_present_holdlook = paste0(screen_out$percent_change_present_holdlook, collapse = ";")
          )
          
          }) %>%
        do.call(rbind, .) %>%
        as.data.frame() ## END: screen type
      
      
      }) %>%
      do.call(rbind, .) %>%
      as.data.frame() ## END: symbol
    
      ## stop cluster
      # parallel::stopCluster(clust)
  

  end_time <- Sys.time()
  cat("Finished stock: ", stock_symbols[i], " at index of: ", i, " at time: ", as.character(end_time), "\n")
  cat("Total time: ", as.character(round(end_time - start_time, 2)), "\n")
  i <<- i + 1
  
  ## week_out
  symbols_out
  
})  %>%
  do.call(rbind, .) %>%
  as.data.frame() ## END: week


cat("Writing Chunk: ", chunk, " to csv", "\n")
write_csv(x = backtest_pattern_screen, 
          path = "data/backtest-screen-pattern-match.csv", 
          append = TRUE)
