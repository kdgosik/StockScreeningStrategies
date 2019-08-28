FROM rocker/shiny:latest
MAINTAINER kdgosik@gmail.com

## Install R packages
COPY install.R /usr/local/src
## install necessary R packages
RUN Rscript /usr/local/src/install.R

