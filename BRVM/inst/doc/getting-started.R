## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",  
  fig.width = 8,
  fig.height = 4.5,
  fig.align = 'center',
  out.width = '95%',
  dpi = 100,
  message = FALSE,
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(BRVM)

## ----brvm_rank----------------------------------------------------------------
BRVM_rank("Top", 10)

