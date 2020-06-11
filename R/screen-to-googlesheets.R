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

## get stocks with sale price between 1 and 5 dollars
stocks <- TTR::stockSymbols() %>%
  as.data.frame() %>%
  filter(LastSale > 1, LastSale < 5, IPOyear < 2020)


## run screen
pattern_screen_out <- sapply(stocks$Symbol, function(l) {
  
  tryCatch({
    pattern_match_screen(l,
                         window_length = 60,
                         comparisons = 10,
                         method = "percent",  # percent, poly, legendre, ns, loess
                         degree = 5,
                         df = 5,
                         span = 0.75 
    )$avg_percent_change > 1.25
  }, error = function(e) FALSE)
  
})


## get symbols that matches the pattern
symbols_out <- names(which(pattern_screen_out))


## create data frame to write to google sheet
df <- data.frame(symbol = symbols_out,
                 buy_date = Sys.Date(),
                 buy_amount = 1,
                 buy_price = stocks$LastSale[stocks$Symbol %in% symbols_out],
                 sell_date = "",
                 sell_amount = "",
                 sell_price = "",
                 type = "dummy",
                 decision_tool = "screen-pattern-match.R",
                 stringsAsFactors = FALSE)

## trade-history
sheet_append(ss = "1WHS3UphexLpMJvStrp0nHhNLyRejy-qZC16q5blSE_Q", 
             sheet = "trade-history", 
             data = df)
