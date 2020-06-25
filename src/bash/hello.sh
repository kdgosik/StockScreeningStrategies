#!/bin/bash

## https://likegeeks.com/linux-bash-scripting-awesome-guide-part5/

## ./src/bash/hello.sh > logs/hello.out &
for i in {1..10}
do
  sleep 1
  echo "Hello World $i"
done