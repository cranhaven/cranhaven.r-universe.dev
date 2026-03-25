## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eider)

## -----------------------------------------------------------------------------
input_table <- data.frame(
  id = c(1, 1, 1, 1, 2, 2, 2, 2),
  diagnosis = c("A", "A", "A", "A", "A", "A", "B", "B")
)

input_table

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/combination1.json"))

## -----------------------------------------------------------------------------
results <- run_pipeline(
  data_sources = list(input_table = input_table),
  feature_filenames = "json_examples/combination1.json"
)

results$features

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/combination2.json"))

## -----------------------------------------------------------------------------
results <- run_pipeline(
  data_sources = list(input_table = input_table),
  feature_filenames = "json_examples/combination2.json"
)

results$features

