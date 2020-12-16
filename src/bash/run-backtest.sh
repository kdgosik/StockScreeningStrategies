#!/bin/bash

## https://medium.com/@robert.i.sandor/getting-started-with-parallelization-in-bash-e114f4353691

script=/home/kirk/Documents/projects/StockScreeningStrategies/src/R/backtest-pattern-match.R
num_processes=10

for chunk in {31..77}
do
  sleep 5
  ((i=i%num_processes)); ((i++==0)) && wait
  Rscript ${script} ${chunk} 5 60 >> logs/run-backtest.out &
done
