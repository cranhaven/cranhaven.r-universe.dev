## ----setup, include = FALSE, echo = FALSE, message=FALSE----------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(rubias)

## -----------------------------------------------------------------------------
ploidies <- check_refmix(chinook, 5)
cpar <- tcf2param_list(chinook, 5, summ = FALSE, ploidies = ploidies)
cpar$RU_starts[1:5]

## -----------------------------------------------------------------------------
cpar$RU_vec[1:15]

