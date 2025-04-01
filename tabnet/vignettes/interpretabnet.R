## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%", 
  out.height = "100%", 
  fig.width = 14,
  fig.height = 12
)

## ----setup--------------------------------------------------------------------
library(tabnet)
library(dplyr)
library(purrr)
library(rsample)
library(yardstick)
library(ggplot2)
library(patchwork)
set.seed(202402)

## -----------------------------------------------------------------------------
ames_split <- initial_split(ames_missing, strata = Sale_Price, prop = 0.8)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

