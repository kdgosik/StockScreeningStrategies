#!/bin/bash

## https://medium.com/@robert.i.sandor/getting-started-with-parallelization-in-bash-e114f4353691

script=/home/kirk/Documents/projects/StockScreeningStrategies/src/R/backtest-pattern-match.R
num_processes=10

for chunk in {21..76}
do
  ((i=i%num_processes)); ((i++==0)) && wait
  Rscript ${script} ${chunk} 5 60 >> logs/run_backtest.out &
done
