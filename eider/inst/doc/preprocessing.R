## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eider)
library(magrittr)

## -----------------------------------------------------------------------------
input_table <- data.frame(
  id = c(1, 1, 1, 1),
  admission_date = as.Date(c(
    "2015-01-01", "2016-01-01", "2016-01-04", "2017-01-01"
  )),
  discharge_date = as.Date(c(
    "2015-01-05", "2016-01-04", "2016-01-08", "2017-01-08"
  )),
  cis_marker = c(1, 2, 2, 3),
  episode_within_cis = c(1, 1, 2, 1),
  diagnosis = c("A", "B", "C", "B")
)

input_table

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/preprocessing1.json"))

## -----------------------------------------------------------------------------
results <- run_pipeline(
  data_sources = list(input_table = input_table),
  feature_filenames = "json_examples/preprocessing1.json"
)

results$features

## -----------------------------------------------------------------------------
processed_table <- input_table %>%
  dplyr::group_by(id, cis_marker) %>%
  dplyr::mutate(
    admission_date = min(admission_date),
    discharge_date = max(discharge_date)
  ) %>%
  dplyr::ungroup()

processed_table

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/preprocessing2.json"))

## -----------------------------------------------------------------------------
results <- run_pipeline(
  data_sources = list(input_table = input_table),
  feature_filenames = "json_examples/preprocessing2.json"
)

results$features

## -----------------------------------------------------------------------------
input_table_with_sum <- input_table %>%
  dplyr::mutate(days = as.numeric(discharge_date - admission_date))

input_table_with_sum

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/preprocessing3.json"))

## -----------------------------------------------------------------------------
results <- run_pipeline(
  data_sources = list(input_table = input_table_with_sum),
  feature_filenames = "json_examples/preprocessing3.json"
)

results$features

