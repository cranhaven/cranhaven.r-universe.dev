## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(retroharmonize)

examples_dir <- system.file("examples", package = "retroharmonize")
my_rds_files <- dir(examples_dir)[grepl(
  ".rds",
  dir(examples_dir)
)]

## ----memory-------------------------------------------------------------------
example_surveys <- read_surveys(file.path(examples_dir, my_rds_files))
documented_surveys <- document_surveys(survey_list = example_surveys)

attr(documented_surveys, "original_list")
documented_surveys

## ----files--------------------------------------------------------------------
document_surveys(survey_paths = file.path(examples_dir, my_rds_files))

