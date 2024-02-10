## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.height = 3.5, 
  fig.width = 6
)

## -----------------------------------------------------------------------------
library(simplevis)

## -----------------------------------------------------------------------------
leaf_sf_col(example_point, 
            col_var = trend_category)

