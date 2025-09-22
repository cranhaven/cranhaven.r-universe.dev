## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mpactr)

## -----------------------------------------------------------------------------
data2 <- import_data(
  example_path("cultures_peak_table.csv"),
  example_path("cultures_metadata.csv"),
  format = "Progenesis"
)


get_peak_table(data2)[, 1:5]

## -----------------------------------------------------------------------------
data2_mispicked <- filter_mispicked_ions(data2,
  ringwin = 0.5,
  isowin = 0.01, trwin = 0.005,
  max_iso_shift = 3, merge_peaks = TRUE,
  merge_method = "sum",
  copy_object = FALSE
)

get_peak_table(data2_mispicked)[, 1:5]

## -----------------------------------------------------------------------------
get_peak_table(data2)[, 1:5]

