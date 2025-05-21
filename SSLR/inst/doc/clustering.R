## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  digits = 3,
  collapse = TRUE,
  comment = "#>"
)
options(digits = 3)

## ----eval=TRUE,  results="hide", warning=FALSE,message=FALSE------------------

library(tidyverse)
library(caret)
library(SSLR)
library(tidymodels)

## ----data, results="hide"-----------------------------------------------------
data(wine)

data <- iris

set.seed(1)
#% LABELED
cls <- which(colnames(iris) == "Species")

labeled.index <- createDataPartition(data$Species, p = .2, list = FALSE)
data[-labeled.index,cls] <- NA


## ----fit, results="hide"------------------------------------------------------
m <- constrained_kmeans() %>% fit(Species ~ ., data)


## ----labels-------------------------------------------------------------------
m %>% cluster_labels()


## ----centers------------------------------------------------------------------
m %>% get_centers()


## ----clusters, warning=FALSE,message=FALSE------------------------------------
library(factoextra)
fviz_cluster(m$model, as.matrix(data[,-cls]))


