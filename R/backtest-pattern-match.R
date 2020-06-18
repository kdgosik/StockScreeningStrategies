library(googlesheets4)
library(tidyverse)
library(magrittr)
library(quantmod)
library(TTR)
library(lubridate)
source("R/screen-pattern-match.R")

## first Monday in 2019 until 74 weeks later
mondays <- ymd("2019-01-07") + 7 * 0:74

## get stocks with sale price between 1 and 5 dollars
stocks <- read_csv("data/google-finance-dump.csv") %>% filter(symbol != "symbol")
symbols <- unique(stocks$symbol)
# stocks <- TTR::stockSymbols() %>%
#   as.data.frame()

# Get all arguments
args <- commandArgs(trailingOnly = TRUE)

cat("Running chunk: ", args[1], "\n")
chunk <- as.numeric(args[1])
chunk_size <- 20
hold_days <- as.numeric(args[2])

## run backtest
i <- 1
backtest_pattern_screen <- lapply(symbols[(1 + chunk_size*(chunk-1)) : (chunk_size*(chunk))], function(s) {
  
  start_time <- Sys.time()
  
  cat("Running screen for stock: ", as.character(s), " at time: ", as.character(start_time), "\n")
  # stockdata <- getSymbols(s, auto.assign = FALSE)
  stockdata <- stocks %>% 
    filter(symbol == s) %>%
    select(date, open, high, low, close) %>%
    as.xts(x = .[,c("open", "high", "low", "close")], order.by =.$date)
  
    ## loop through weeks for each stock
    week_out <- lapply(mondays, function(m) {
      ## run screen
      cat("Running screen for day: ", as.character(m), "\n")
    
      ## get stock data from beginning until the monday being studied
      stockdata_hist <- stockdata[paste0("/", m)]
      # stockdata_hist <- stockdata %>% 
      #   filter(date <= m) %>%
      #   select(date, open, high, low, close, volume, adjusted) %>%
      #   as.xts(x = .[,c("open", "high", "low", "close")], order.by =.$date)
    
      screen_type <- c("percent", "poly", "legendre", "ns", "loess")
      lapply( screen_type, function(st) {
        cat("Running screen using type: ", st, "\n")

        screen_out <- tryCatch({
          
          window_length <- 60
          comparisons <- 10
          degree <- 5
          degfree <- 5
          span <- 0.75
          parameters <- paste0("window_length:", window_length, 
                               "; hold_days:", hold_days,
                               "; comparisons:", comparisons, 
                               "; degree:", degree,
                               "; df:", degfree,
                               "; span:", span,
                               "; chunk: ", chunk,
                               "; chunk_size: ", chunk_size)
          
          screen_pattern_match(stockdata = stockdata_hist,
                               window_length = window_length,
                               hold_days = hold_days,
                               comparisons = comparisons,
                               method = st,  # percent, poly, legendre, ns, loess
                               degree = degree,
                               df = degfree,
                               span = span)
          },
          error = function(e) list(avg_percent_change = 0, proportion_up = 0))
    
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
                     screen_avg_change = screen_out$avg_percent_change,
                     screen_prop_up = screen_out$proportion_up)
          
          }) %>%
        do.call(rbind, .) %>%
        as.data.frame() ## END: screen type
      
      
      }) %>%
      do.call(rbind, .) %>%
      as.data.frame() ## END: screen day
  

  end_time <- Sys.time()
  cat("Finished stock: ", s, " at index of: ", i, " at time: ", as.character(end_time), "\n")
  cat("Total time: ", as.character(round(end_time - start_time, 2)), "\n")
  i <<- i + 1
  
  week_out
  
})  %>%
  do.call(rbind, .) %>%
  as.data.frame() ## END: stock


cat("Writing Chunk: ", chunk, " to csv", "\n")
# write.csv(backtest_pattern_screen, paste0('backtest-pattern-screen-hold-', hold_days, '-chunk-', chunk, '.csv'))
write_csv(x = backtest_pattern_screen, 
          path = "data/backtest-screen-pattern-match.csv", 
          append = TRUE,
          col_names = TRUE)
