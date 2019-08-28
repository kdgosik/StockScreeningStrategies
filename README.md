## Building Container


### build_docker.sh

```{bash}
bash build_docker.sh
```

### install.R

```{r}
## CRAN Packages
install.packages("ggplot2")
install.packages("quantmod")
install.packages("TTR")
install.packages("dygraphs")
install.packages("dplyr")
install.packages("data.table")
install.packages("magrittr")
```



## Pushing to Dockerhub

```{bash}
bash push_docker.sh
```


## Running Container


```{bash}
bash run_docker.sh
```


## Running Container on Cloud Run

[![Run on Google Cloud](https://storage.googleapis.com/cloudrun/button.svg)](https://console.cloud.google.com/cloudshell/editor?shellonly=true&cloudshell_image=gcr.io/cloudrun/button&cloudshell_git_repo=https://github.com/kdgosik/StockScreeningStrategies)