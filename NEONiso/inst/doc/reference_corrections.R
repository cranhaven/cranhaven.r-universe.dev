## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(NEONiso)

## ----echo = FALSE-------------------------------------------------------------
#manually load data file:
carb <- readRDS("carb.rds")
print(carb[, -4], width = Inf, n = 27)

