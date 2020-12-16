knitr::opts_chunk$set(echo = TRUE)
library(googlesheets4)
library(tidyverse)
library(data.table)
library(magrittr)
library(quantmod)
library(TTR)
library(plotly)
library(igraph)
library(visNetwork)


data_path <- "~/Documents/projects/StockScreeningStrategies/data/"
backtest <- read_csv(paste0(data_path, "backtest-screen-pattern-match.csv"), col_types = "DDcddcccccccc")

if( !{file.exists( paste0(data_path, "backtest-hypertune.csv") )} ) {
  write_csv(x = data.frame("buy_date", "gross", "total", "return", "cumprod", "screen_type",  
                           "topn_cutoff", "screen_metric", "screen_change_cutoff", "num_patterns", "price_cutoff"),
            path = paste0(data_path, "backtest-hypertune.csv"),
            col_names = FALSE)
}


## helpers.R

to_avg <- function(x) {
  sapply(x, function(y) mean(as.numeric(unlist(strsplit(y, ";")))), USE.NAMES = FALSE)
}


to_prop <- function(x) {
  sapply(x, function(y) mean(as.numeric(unlist(strsplit(y, ";"))) > 1), USE.NAMES = FALSE)
}


unique_patterns <- function(x) {
  
  sapply(x, function(y) {
    
    idx <- as.numeric(unlist(strsplit(y, ";")))
    sum(idx[2:10] - idx[1:9] > 1) + 1
    
  }, USE.NAMES = FALSE)
  
}


# Get all arguments
args <- commandArgs(trailingOnly = TRUE)

start_time <- Sys.time()

cat("## Running with screen type: ", args[1], "with screen metric", args[2], "at time", as.character(start_time), "\n")

st <- args[1]  ## screen_type <- c("percent", "poly", "legendre", "ns", "loess")
sm <- args[2]  ## screen_metric <- c("percent_change_window_hold", "percent_change_window_holdlook", "percent_change_present_hold", "percent_change_present_holdlook")


topn_cutoff <- 1:20
screen_change_cutoff <- seq(1, 1.25, 0.05)
num_patterns <- 2:7
price_cutoff <- c(2, 3, 4, 5, 10, 20, 50, 100, 1000, 1e6)
norm_cutoff <- seq(1.5, 3, by= 0.1)



backtest <- backtest %>%
  rename(screen_change = ends_with(match = sm)) %>% ## screen_metric (sm)
  filter(monday_open > 0, friday_close > 0) %>%
  mutate(percent_change = friday_close / monday_open) %>%
  mutate(screen_avg_change = to_avg( screen_change ),
         screen_prop_up = to_prop( screen_change ),
         unique_patterns = unique_patterns( index ),
         norm_avg_value = to_avg( norm_values ))
  
  

hypertune_out <- lapply(topn_cutoff, function(n) {
  
  cat("Using a cutoff for the top,", as.character(n), "stocks \n")
    
    lapply(screen_change_cutoff, function(sc) {
      
      cat("Running with a screen change cutoff of: ", as.character(sc), "\n")
      
      lapply(num_patterns, function(np) {
        
        cat("Requiring a ", as.character(np), "number of patterns \n")
        
        lapply(price_cutoff, function(pc) {
            
            ## testing parameters     
            # st <- "percent"
            # n <- 1
            # sm <- "percent_change_present_hold"
            # sc <- 1.1
            # np <- 5
            # pc <- 1e6
          
          cat("Running with a price cutoff less than ", as.character(pc), "\n")
            
            out <- backtest %>%
              filter(screen_prop_up  == 1,
                     screen_type == st, ## screen_type (st)
                     unique_patterns >= np,  ## num_patterns (np)
                     screen_avg_change > sc, ## screen_change (sc)
                     monday_open < pc) %>% ## price_cutoff (pc)
              group_by( buy_date ) %>% 
              arrange( -screen_avg_change ) %>%
              slice_head( n = n ) %>%  ## topn_cutoff (n)
              group_by( buy_date ) %>% 
              summarise( gross = sum(percent_change), total = n(), return = gross/total ) %>% 
              ungroup %>%
              mutate(cumprod = round(cumprod(return), 4),
                     screen_type = st,
                     topn_cutoff = n,
                     screen_metric = sm,
                     screen_change_cutoff = sc,
                     num_patterns = np,
                     price_cutoff = pc
              ) 
            
            ## output backtest summary
            out
            
            
          }) %>% ## END: price_cutoff
            do.call(rbind, .)
          
        }) %>% ## END: num_patterns
          do.call(rbind, .)
      
    }) %>% ## END: screen_change
      do.call(rbind, .)
    
  }) %>% ## END: topn_cutoff
    do.call(rbind, .)


write_csv(hypertune_out,  paste0(data_path, "backtest-hypertune.csv"), append = TRUE)
