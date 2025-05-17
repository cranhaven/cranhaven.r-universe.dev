## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lotterybr)

## -----------------------------------------------------------------------------
quina_ganhadores = get_data("quina","winners","ptbr")
lotofacil_numbers = get_data("lotofacil","numbers","eng")

