#!/bin/sh

set -ev

VERSION=`cat VERSION.txt`

docker build -t kdgosik/stockscreen:$VERSION .
docker build -t kdgosik/stockscreen:latest .