## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eider)
library(magrittr)

## -----------------------------------------------------------------------------
ae_data_filepath <- eider_example("random_ae_data.csv")

ae_data_filepath

## -----------------------------------------------------------------------------
ae_data <- utils::read.csv(ae_data_filepath) %>%
  dplyr::mutate(date = as.Date(date))

dplyr::glimpse(ae_data)

## -----------------------------------------------------------------------------
ae_count_filepath <- eider_example("ae_total_attendances.json")
writeLines(readLines(ae_count_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(ae = ae_data_filepath),
  feature_filenames = ae_count_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
ae_neurology_filepath <- eider_example("ae_attendances_neurology_2017.json")
writeLines(readLines(ae_neurology_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(ae = ae_data_filepath),
  feature_filenames = c(ae_count_filepath, ae_neurology_filepath)
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
ae_data %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(total_ae_attendances = dplyr::n()) %>%
  dplyr::glimpse()

## -----------------------------------------------------------------------------
ae_data %>%
  dplyr::filter(date >= "2017-01-01", date <= "2017-12-31") %>%
  dplyr::filter(diagnosis_1 == 13 | diagnosis_2 == 13 | diagnosis_3 == 13) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(total_neurology_ae_attendances = dplyr::n()) %>%
  dplyr::glimpse()

## -----------------------------------------------------------------------------
ae_present_filepath <- eider_example("has_visited_ae.json")
writeLines(readLines(ae_present_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(ae = ae_data_filepath),
  feature_filenames = ae_present_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
days_since_ae_filepath <- eider_example("days_since_last_ae.json")
writeLines(readLines(days_since_ae_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(ae = ae_data_filepath),
  feature_filenames = days_since_ae_filepath
)

dplyr::glimpse(res$features)

