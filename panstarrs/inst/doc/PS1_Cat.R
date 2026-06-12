## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(panstarrs)
library(dplyr)
library(data.table)

## -----------------------------------------------------------------------------
coords <- ps1_mast_resolve('KQ Uma')
coords

## -----------------------------------------------------------------------------
df_cone <- ps1_cone(
  coords$ra, 
  coords$decl,
  r_arcmin = 0.01, 
  table = 'mean',
  release = 'dr2'
)

# tidyverse approach
df_cone |>
  dplyr::select(dplyr::matches('[grizy]MeanPSFMag$'))

# or if you prefer data.table approach
df_cone[, .SD, .SDcols = grepl('[grizy]MeanPSFMag$', names(df_cone))]

