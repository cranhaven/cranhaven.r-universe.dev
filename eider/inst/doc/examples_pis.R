## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eider)
library(magrittr)

## -----------------------------------------------------------------------------
pis_data_filepath <- eider_example("random_pis_data.csv")

pis_data_filepath

## -----------------------------------------------------------------------------
pis_data <- utils::read.csv(pis_data_filepath) %>%
  dplyr::mutate(paid_date = lubridate::ymd(paid_date))

dplyr::glimpse(pis_data)

## -----------------------------------------------------------------------------
unique_bnf_filepath <- eider_example("distinct_bnf_prescriptions.json")
writeLines(readLines(unique_bnf_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(pis = pis_data_filepath),
  feature_filenames = unique_bnf_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
drugs_since_2016_filepath <- eider_example("num_prescriptions_since_2016.json")
writeLines(readLines(drugs_since_2016_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(pis = pis_data_filepath),
  feature_filenames = drugs_since_2016_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
max_items_filepath <- eider_example("max_drugs_in_transaction.json")
writeLines(readLines(max_items_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(pis = pis_data_filepath),
  feature_filenames = max_items_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
max_items_day_filepath <- eider_example("max_drugs_in_day.json")
writeLines(readLines(max_items_day_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(pis = pis_data_filepath),
  feature_filenames = c(max_items_filepath, max_items_day_filepath)
)

dplyr::glimpse(res$features)

