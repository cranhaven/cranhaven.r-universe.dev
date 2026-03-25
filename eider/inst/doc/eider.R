## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eider)

## -----------------------------------------------------------------------------
example_table <- data.frame(
  patient_id        = c(1, 1, 1, 2, 2, 3, 3, 3),
  attendance_reason = c(6, 6, 7, 6, 6, 7, 7, 7)
)

data_sources <- list(attendances = example_table)

## -----------------------------------------------------------------------------
data_sources_2 <- list(attendances = "attendances.csv")

## -----------------------------------------------------------------------------
data_sources_3 <- list(
  attendances = example_table,   # A variable which has already been constructed
  other_data = "other_data.csv"  # A file to be read in
)

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/eider.json"))

## -----------------------------------------------------------------------------
run_pipeline(
  data_sources = data_sources,
  feature_filenames = "json_examples/eider.json"
)

## -----------------------------------------------------------------------------
json_string <- '{
    "transformation_type": "count",
    "source_table": "attendances",
    "grouping_column": "patient_id",
    "absent_default_value": 0,
    "output_feature_name": "total_attendances",
    "filter": {
        "column": "attendance_reason",
        "type": "in",
        "value": [6]
    }
}'

run_pipeline(
  data_sources = data_sources,
  feature_filenames = json_string
)

