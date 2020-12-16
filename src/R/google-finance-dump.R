library(googlesheets4)
library(tidyverse)
library(magrittr)
library(quantmod)
library(TTR)
library(lubridate)

## get stocks with sale price
stocks <- TTR::stockSymbols() %>%
  as.data.frame()
write_csv(x = data.frame("date", "symbol", "open", "high", "low", "close", "volume", "adjusted"), 
          path = "data/google-finance-dump.csv", 
          col_names = FALSE)

end_chunks <- NROW(stocks) %/% 5 + 1
for( chunk in 1 : end_chunks ) {
  
  cat("Running chunk: ", chunk, "\n")
  chunk_symbols <- stocks$Symbol[(1 + 5 * (chunk - 1)) : (5 * (chunk))]
  

  out_list <- lapply(chunk_symbols, function(s) tryCatch({getSymbols(s, auto.assign = FALSE)}, error=function(e) e))
  
  for ( i in seq_along(out_list) ) {
    
    if( NCOL(out_list[[i]]) > 1 ) {
    
      # out <- eval(parse(text = s))["2010-01-01/"]
      out <- out_list[[i]]
    
      colnames(out) <- c("open", "high", "low", "close", "volume", "adjusted")[1 : NCOL(out)]
      date_vector <- date(out)
    
      if( !{"adjusted" %in% colnames(out)} ) out$adjusted <- 0
    
      out <- out %>%
        as_tibble %>%
        mutate( date = date_vector, symbol = chunk_symbols[i] ) %>%
        select(date, symbol, open, high, low, close, volume, adjusted)
    
      write_csv(x = out, 
                path = "data/google-finance-dump.csv", 
                append = TRUE)
      
    }
  
  }
  
  rm(list = chunk_symbols)
  
}
