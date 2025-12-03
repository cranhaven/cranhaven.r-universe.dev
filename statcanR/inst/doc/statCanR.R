## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(statcanR)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("warint/statcanR")

## -----------------------------------------------------------------------------
library(statcanR)

## ---- eval=FALSE--------------------------------------------------------------
#  # Identify with statcan_search() function
#  statcan_search(c("federal","expenditures"),"eng")

## ---- eval=FALSE--------------------------------------------------------------
#  # Get data with statcan_data function
#  mydata <- statcan_download_data("27-10-0014-01", "eng")

