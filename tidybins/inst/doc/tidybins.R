## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------


library(tidybins)
suppressPackageStartupMessages(library(dplyr))

## -----------------------------------------------------------------------------

tibble::tibble(SALES = as.integer(rnorm(1000L, mean = 10000L, sd = 3000))) -> sales_data

sales_data %>% 
  bin_cols(SALES, bin_type = "value") -> sales_data1

sales_data1

## -----------------------------------------------------------------------------
sales_data1 %>% 
  bin_summary() %>% 
  print(width = Inf)

