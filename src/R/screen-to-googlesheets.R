## install.packages("googlesheets4")
## install.packages("devtools")
## devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(dplyr)
library(magrittr)
library(quantmod)
library(TTR)
library(lubridate)

## clear history
# rm(list = ls()); gc(reset = TRUE)

## Authorize Googlesheets Access
## googlesheets4::gs4_auth()

## source screen pattern match
source("src/R/screen-pattern-match.R")
# source("R/screen-changepoint.R")
# source("R/screen-sma.R")
# source("R/screen-ema.R")
# source("R/utils.R")

## get stock symbols
stocks <- read_csv("data/stock-symbols.csv")
stock_symbols <- stocks$Symbol


## finding the closet upcoming Monday date
this_monday <- as.character(Sys.Date()+0:6)[wday(Sys.Date()+0:6)==2]

## run screen
# cores <- detectCores() - 2
# cl <- parallel::makeCluster(cores)
# doParallel::registerDoParallel(cl)
# foreach_packages <- c("quantmod", "TTR", "splines", "pracma")
# foreach_exports <- c("Legendre", "screen_pattern_match")
# screen_pattern_out <- foreach(s = stock_symbols,
#                               .packages = foreach_packages,
#                               .export = foreach_exports) %dopar% {
screen_pattern_out <- sapply(stock_symbols[stock_symbols %in% stocks$Symbol[1:1000]], function(s) {
  
  cat("Running for stock: ", s, "\n")
  stockdata <- tryCatch({getSymbols(s, auto.assign = FALSE)["/2020-06-19"]}, error=function(e) e)
  
  if( NCOL(stockdata) > 1 ) {
    tryCatch({
      screen_pattern_match(stockdata = stockdata,
                           window_length = 60,
                           look_ahead_window = 60,
                           hold_days = 5,
                           comparisons = 10,
                           method = "ns",  # percent, poly, legendre, ns, loess
                           degree = 5,
                           df = 5,
                           span = 0.75)
    }, error = function(e) e)
  }
  
}, USE.NAMES = TRUE, simplify = FALSE)
# }

## filtering screen data
screen_out_filtered <- Filter(function(s) { mean(s$percent_change_window_hold) > 1.1 }, screen_pattern_out)
screen_out_filtered <- Filter(function(s) { mean(s$percent_change_window_hold > 1) == 1 }, screen_out_filtered)
screen_out_filtered <- Filter(function(s) { NCOL(s$data) > 1 }, screen_out_filtered)


screen_df <- Map(function(s){
  
  data.frame(
    symbol = gsub(".Open","", colnames(s$data)[1]),
    buy_date = this_monday,
    buy_amount = 1,
    buy_price = as.numeric(last(s$data[,4])),
    sell_date = "",
    sell_amount = "",
    sell_price = "",
    type = "dummy",
    decision_tool = "screen-pattern-match.R",
    avg_percent_change = mean(s$percent_change_window_hold),
    proportion_up = mean(s$percent_change_window_hold > 1),
    stringsAsFactors = FALSE
  )
}, screen_out_filtered) %>%
  do.call(rbind, .) %>%
  filter(avg_percent_change > 1.1, proportion_up == 1) %>%
  arrange(-avg_percent_change) %>%
  mutate(week_rank = 1:n()) %>%
  slice_head(n = 20)



## add screen data to screen-history tab of google sheet
sheet_append(ss = "1opnAxpzlJ-2HoJqLDb1Z8fImUTiekDeQkceaONfEvUA",
             sheet = "screen-history", 
             data = screen_df)



## create reports from Rmd files
lapply(screen_df$symbol, function(s) {
  rmarkdown::render("notebooks/report-screen-stock.Rmd", params = list(
    symbol = s,
    window_length = 60,
    look_ahead_window = 60,
    hold_days = 5
  ),
  output_dir = "docs",
  output_file = paste0(this_monday, "-", s, ".html"))
})


## upload to googledrive
lapply(screen_df$symbol, function(s) {
  googledrive::drive_upload(
    media = paste0("docs/", this_monday, "-", s, ".html"),
    path = paste0("hedgefund/", this_monday, "-", s, ".html")
  )
})

