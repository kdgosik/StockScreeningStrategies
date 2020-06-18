## install.packages("googlesheets4")
# install.packages("devtools")
## devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(dplyr)
library(magrittr)
library(quantmod)
library(TTR)

## clear history
# rm(list = ls()); gc(reset = TRUE)

## Authorize Googlesheets Access
## googlesheets4::gs4_auth()

## source pattern match screener
source("R/screen-pattern-match.R")
# source("R/screen-changepoint.R")
# source("R/screen-sma.R")
# source("R/screen-ema.R")
# source("R/utils.R")

## get stocks with sale price between 1 and 20 dollars
stocks <- TTR::stockSymbols() %>%
  dplyr::filter(LastSale > 1, LastSale < 20, IPOyear < 2020)
symbols <- stocks$Symbol


stocks <- read_csv("data/google-finance-dump.csv") %>% filter(symbol != "symbol", close > 2, close <= 20)
symbols <- unique(stocks$symbol)

## run screen
pattern_screen_out <- sapply(symbols, function(s) {
  
  cat("Running for stock: ", s, "\n")
  stockdata <- tryCatch({getSymbols(s, auto.assign = FALSE)}, error=function(e) e)
  
  if( NCOL(stockdata) > 1 ) {
    tryCatch({
      screen_pattern_match(stockdata = stockdata,
                           window_length = 60,
                           hold_days = 5,
                           comparisons = 10,
                           method = "ns",  # percent, poly, legendre, ns, loess
                           degree = 5,
                           df = 5,
                           span = 0.75)
    }, error=function(e) e)
  }
  
}, USE.NAMES = TRUE, simplify = FALSE)


## filtering screen data
screen_out_filtered <- Filter(function(s) s$avg_percent_change > 1.1 s$proportion_up == 1, pattern_screen_out)

## get symbols that matches the pattern
symbols_out <- names(which(pattern_screen_out))


## create data frame to write to google sheet
df <- data.frame(symbol = symbols_out,
                 buy_date = Sys.Date()+1,
                 buy_amount = 1,
                 buy_price = stocks$LastSale[stocks$Symbol %in% symbols_out],
                 sell_date = "",
                 sell_amount = "",
                 sell_price = "",
                 type = "dummy",
                 decision_tool = "screen-pattern-match.R",
                 stringsAsFactors = FALSE)

## trade-history
sheet_append(ss = "1opnAxpzlJ-2HoJqLDb1Z8fImUTiekDeQkceaONfEvUA", 
             sheet = "trade-history", 
             data = df)
