## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eider)
library(magrittr)

## -----------------------------------------------------------------------------
ltc_data_filepath <- eider_example("random_ltc_data.csv")

ltc_data_filepath

## -----------------------------------------------------------------------------
ltc_data <- utils::read.csv(ltc_data_filepath) %>%
  dplyr::mutate(asthma = lubridate::ymd(asthma),
                diabetes = lubridate::ymd(diabetes),
                parkinsons = lubridate::ymd(parkinsons))

dplyr::glimpse(ltc_data)

## -----------------------------------------------------------------------------
years_asthma_filepath <- eider_example("years_with_asthma.json")
writeLines(readLines(years_asthma_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(ltc = ltc_data_filepath),
  feature_filenames = years_asthma_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
has_asthma_filepath <- eider_example("has_asthma.json")
writeLines(readLines(has_asthma_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(ltc = ltc_data_filepath),
  feature_filenames = has_asthma_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
num_ltcs_filepath <- eider_example("number_of_ltcs.json")
writeLines(readLines(num_ltcs_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(ltc = ltc_data_filepath),
  feature_filenames = num_ltcs_filepath
)

dplyr::glimpse(res$features)

