## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
options(cli.unicode = FALSE)

## ----install, eval = FALSE----------------------------------------------------
#  install.packages("bupaverse")

## ----load---------------------------------------------------------------------
library(bupaverse)

## ----example------------------------------------------------------------------
patients %>%
  processing_time(level = "activity") %>%
  plot()

