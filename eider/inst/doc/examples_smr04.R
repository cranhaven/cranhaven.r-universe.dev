## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eider)
library(magrittr)

## -----------------------------------------------------------------------------
smr04_data_filepath <- eider_example("random_smr04_data.csv")

smr04_data_filepath

## -----------------------------------------------------------------------------
smr04_data <- utils::read.csv(smr04_data_filepath) %>%
  dplyr::mutate(
    admission_date = lubridate::ymd(admission_date),
    discharge_date = lubridate::ymd(discharge_date)
  )

dplyr::glimpse(smr04_data)

## -----------------------------------------------------------------------------
pt_episodes_filepath <- eider_example("psychotherapy_episodes.json")
writeLines(readLines(pt_episodes_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(smr04 = smr04_data_filepath),
  feature_filenames = pt_episodes_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
pt_stays_filepath <- eider_example("psychotherapy_stays.json")
writeLines(readLines(pt_stays_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(smr04 = smr04_data_filepath),
  feature_filenames = pt_stays_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
smr04_with_days_data <- smr04_data %>%
  dplyr::mutate(days_in_hospital = as.numeric(discharge_date - admission_date))

dplyr::glimpse(smr04_with_days_data)

## -----------------------------------------------------------------------------
total_days_filepath <- eider_example("days_in_smr04.json")
writeLines(readLines(total_days_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(smr04_with_days = smr04_with_days_data),
  feature_filenames = total_days_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
longest_stay_filepath <- eider_example("longest_stay.json")
writeLines(readLines(longest_stay_filepath))

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(smr04_with_days = smr04_with_days_data),
  feature_filenames = longest_stay_filepath
)

dplyr::glimpse(res$features)

## -----------------------------------------------------------------------------
res <- run_pipeline(
  data_sources = list(
    smr04 = smr04_data_filepath,
    smr04_with_days = smr04_with_days_data
  ),
  feature_filenames = c(
    pt_episodes_filepath,
    pt_stays_filepath,
    total_days_filepath,
    longest_stay_filepath
  )
)

dplyr::glimpse(res$features)

