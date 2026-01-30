## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(retroharmonize)

## ----systemfiles--------------------------------------------------------------
examples_dir <- system.file("examples", package = "retroharmonize")
survey_files <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
survey_files

## ----eval=FALSE---------------------------------------------------------------
# example_surveys <- read_surveys(
#   file.path(examples_dir, survey_files),
#   export_path = tempdir()
# )

## -----------------------------------------------------------------------------
example_surveys <- read_surveys(
  survey_paths = file.path(examples_dir, survey_files),
  export_path = NULL
)

## -----------------------------------------------------------------------------
ZA5913_survey <- example_surveys[[1]]
# A small subset of this survey
head(ZA5913_survey[, c(1, 4, 5, 34)])

## -----------------------------------------------------------------------------
attributes(ZA5913_survey)

## -----------------------------------------------------------------------------
document_surveys(survey_paths = file.path(examples_dir, survey_files))

## ----metadata-----------------------------------------------------------------
metadata_create(example_surveys) %>% head()

