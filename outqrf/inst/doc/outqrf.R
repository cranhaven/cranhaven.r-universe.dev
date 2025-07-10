## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 6, 
  fig.height = 4,
  fig.align = "center"
)

## ----install,eval=FALSE-------------------------------------------------------
#  # Development version
#  devtools::install_github("flystar233/outqrf")

## ----usage, echo=TRUE---------------------------------------------------------
library(outqrf)
#Generate data with outliers in numeric columns
irisWithOutliers <- generateOutliers(iris, p = 0.05,seed =2024)
# Find outliers by quantile random forest regressions
out <- outqrf(irisWithOutliers,quantiles_type=400)
out$outliers

## ----Evaluation1, echo=TRUE---------------------------------------------------
library(outqrf)
irisWithOutliers <- generateOutliers(iris, p = 0.05,seed =2024)
qrf <- outqrf(irisWithOutliers,quantiles_type=400)

evaluateOutliers(iris,irisWithOutliers,qrf$outliers)

## ----Evaluation1_1, eval=FALSE------------------------------------------------
#  plot(qrf)

## ----Evaluation1_2, echo=FALSE------------------------------------------------
library(outqrf)
irisWithOutliers <- generateOutliers(iris, p = 0.05,seed =2024)
qrf <- outqrf(irisWithOutliers,quantiles_type=400)
plot(qrf)

## ----Evaluation2, echo=TRUE---------------------------------------------------
library(outqrf)
library(ggplot2)
library(dplyr)
data <- diamonds|>select(price,carat,cut,color,clarity)
data2 <- outqrf::generateOutliers(data, p = 0.001,seed =2024)
# 108
qrf <- outqrf(data2,num.threads=8,quantiles_type=400)
#The process can be slow because it needs to predict the value at 400|1000 quantiles for each observation. 
evaluateOutliers(data,data2,qrf$outliers)

