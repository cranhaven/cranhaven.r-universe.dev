## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(confidenceSim)
library(dplyr)
library(parallel)
library(pbapply)
set.seed(613)

requireNamespace("plyr", quietly = TRUE)

