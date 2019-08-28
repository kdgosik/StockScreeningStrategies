#!/bin/sh

set -ev

VERSION=`cat VERSION.txt`

docker push kdgosik/stockscreen:${VERSION}
docker push kdgosik/stockscreen:latest
