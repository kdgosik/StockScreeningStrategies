library(ggplot2)
library(plotly)
library(dygraphs)
library(dplyr)
library(magrittr)
library(quantmod)
library(TTR)

rm(list = ls()); gc(reset = TRUE)

source("R/screen-pattern-match.R")
source("R/screen-changepoint.R")
source("R/screen-sma.R")
source("R/screen-ema.R")
source("R/utils.R")


getSymbols(c("HMY", "NVDA"))
loadSymbols(c("HMY", "NVDA"))
loadSymbols(c('BTCUSD=X','GLD', 'SPY'))
df <- Reduce(merge, list(Cl(`BTCUSD=X`), Cl(GLD), Cl(SPY)))
df <- df["2012/"] # from 2012 to present
df <- scale(df)

df_returns <- Reduce(merge, list(dailyReturn(`BTCUSD=X`), dailyReturn(GLD), dailyReturn(SPY))) %>% 
  as.data.frame() %>% 
  rename(BTC=daily.returns, GLD=daily.returns.1, SPY=daily.returns.2)

# loadSymbols(c('BTCUSD=X','AMD'))
# btc_returns <- data.frame(Open=dailyReturn(Op(`BTCUSD=X`)), 
#                           High=dailyReturn(Hi(`BTCUSD=X`)), 
#                           Low=dailyReturn(Lo(`BTCUSD=X`)), 
#                           Close=dailyReturn(Cl(`BTCUSD=X`))) %>% 
#   rename(Open=daily.returns, High=daily.returns.1, Low=daily.returns.2,Close=daily.returns.3) %>% 
#   as.xts


## Testing Functions ###################


## Testing: Pattern Match ####################################3

test_points <- 40
out <- pattern_match_screen(NVDA[1:(NROW(NVDA) - test_points),], method="loess")

test_data <- NVDA[(NROW(NVDA) - test_points) : NROW(NVDA),]
test_data <- data.frame(date = index(test_data), price = as.numeric(Cl(test_data)))

end <- NROW(out$data)
win <- 60
plot_data <- data.frame(date = c(index(out$data)[(end - (win - 1)) : end], seq(index(out$data)[end],index(out$data)[end]+test_points, by = 1)),
                        price = c(as.numeric(Cl(out$data)[(end - (win - 1)) : end]), test_data$price),
                        pattern1 = as.numeric(Cl(out$data)[out$index[1]:(out$index[1] + win + test_points)]),
                        pattern2 = as.numeric(Cl(out$data)[out$index[2]:(out$index[2] + win + test_points)]),
                        pattern3 = as.numeric(Cl(out$data)[out$index[3]:(out$index[3] + win + test_points)]),
                        pattern4 = as.numeric(Cl(out$data)[out$index[4]:(out$index[4] + win + test_points)]),
                        pattern5 = as.numeric(Cl(out$data)[out$index[5]:(out$index[5] + win + test_points)]))



plot_data <- plot_data %>%
  mutate(price = scale(price),
         pattern1 = scale(pattern1),
         pattern2 = scale(pattern2),
         pattern3 = scale(pattern3),
         pattern4 = scale(pattern4),
         pattern5 = scale(pattern5),
         color = ifelse(date >= min(test_data$date), "red","black"))


ggplot(plot_data, aes(x = date, y = price, color = I(color))) +
  geom_point() +
  geom_smooth(span = 0.5) +
  geom_smooth(aes(x = date, y = pattern1), color = "darkgreen", span = 0.5) +
  geom_smooth(aes(x = date, y = pattern2), color = "darkblue", span = 0.5) +
  geom_smooth(aes(x = date, y = pattern3), color = "darkorange", span = 0.5) +
  geom_smooth(aes(x = date, y = pattern4), color = "darkred", span = 0.5) + 
  geom_smooth(aes(x = date, y = pattern5), color = "darkred", span = 0.5) 


cor(plot_data[plot_data$color=="black", c('price','pattern1','pattern2','pattern3','pattern4')])



pattern_screen_out <- sapply(stock_list, function(l) {
  try({
    pattern_match_screen(na.omit(eval(parse(text = l))),
                         window_length = 60,
                         comparisons = 10,
                         method = "percent",  # percent, poly, legendre, ns, loess
                         degree = 5,
                         df = 5,
                         span = 0.75 
                         )$avg_percent_change > 1.2
  })
})

table(pattern_screen_out)

## Testing: SMA ############################################################

getSymbols(stocks$Symbol[101:NROW(stocks)])

stock_list <- grep("[A-Z]{2,3}", ls(), value = TRUE)

# sma_screen_out <- sapply(stock_list, function(l) try(perform_sma_screen(l)))
# table(sma_screen_out)

sma_screen_out <- sapply(stock_list, function(l) {
  
  try({
    back_test_sma(na.omit(eval(parse(text=l))),
                  short = 20,
                  long = 100,
                  cor_period = 10
                  )$buy
    })
  
})

table(sma_screen_out)

rm(list = stock_list)


# SEAC
## Testing: EMA ############################################################

ema_screen_out <- sapply(stock_list, function(l) {
  
  try({
    back_test_ema(na.omit(eval(parse(text=l))),
                  short = 20,
                  long = 100,
                  cor_period = 10
    )$buy
  })
  
})

table(ema_screen_out)

rm(list = stock_list)
