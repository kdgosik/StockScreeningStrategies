#!/bin/bash

## https://medium.com/@robert.i.sandor/getting-started-with-parallelization-in-bash-e114f4353691

script=/home/kirk/Documents/projects/StockScreeningStrategies/src/R/backtest-hypertune.R
num_processes=4

for st in percent poly legendre ns loess
do
  for sm in percent_change_window_hold percent_change_window_holdlook percent_change_present_hold percent_change_present_holdlook
  do
  
  sleep 5
  ((i=i%num_processes)); ((i++==0)) && wait
  Rscript ${script} ${st} ${sm} >> logs/run-backtest-hypertune.out &
  
  done
  
done
