## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eider)

## -----------------------------------------------------------------------------
input_table <- data.frame(
  id = c(1, 1, 1, 2, 3),
  date = as.Date(c(
    "2024-01-01", "2024-01-02", "2024-01-03",
    "2023-01-01", "2023-01-01"
  )),
  diagnosis = c("A", "B", "C", "B", "C")
)
input_table

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/filter1.json"))

## -----------------------------------------------------------------------------
results <- run_pipeline(
  data_sources = list(input_table = input_table),
  feature_filenames = "json_examples/filter1.json"
)

results$features

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/filter2.json"))

## -----------------------------------------------------------------------------
results <- run_pipeline(
  data_sources = list(input_table = input_table),
  feature_filenames = "json_examples/filter2.json"
)

results$features

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/filter3.json"))

## -----------------------------------------------------------------------------
results <- run_pipeline(
  data_sources = list(input_table = input_table),
  feature_filenames = "json_examples/filter3.json"
)

results$features

