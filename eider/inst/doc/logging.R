## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eider)

## ----eval=FALSE---------------------------------------------------------------
#  logger::log_threshold(logger::DEBUG)

## ----eval=FALSE---------------------------------------------------------------
#  logger::log_threshold(logger::TRACE)

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/logging1.json"))

## ----error=TRUE---------------------------------------------------------------
run_pipeline(
  data_sources = list(ae2 = eider_example("random_ae_data.csv")),
  feature_filenames = "json_examples/logging1.json"
)

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/logging2.json"))

## ----error=TRUE---------------------------------------------------------------
run_pipeline(
  data_sources = list(ae2 = eider_example("random_ae_data.csv")),
  feature_filenames = "json_examples/logging2.json"
)

## ----comment='', echo=FALSE---------------------------------------------------
writeLines(readLines("json_examples/logging3.json"))

## ----error=TRUE---------------------------------------------------------------
run_pipeline(
  data_sources = list(ae2 = eider_example("random_ae_data.csv")),
  feature_filenames = "json_examples/logging3.json"
)

