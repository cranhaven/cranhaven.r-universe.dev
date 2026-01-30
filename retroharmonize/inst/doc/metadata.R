## ----knitropts, include = FALSE-----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(retroharmonize)
library(dplyr)

## ----surveyfiles--------------------------------------------------------------
examples_dir <- system.file("examples", package = "retroharmonize")
survey_files <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
survey_files

## ----readfiles----------------------------------------------------------------
survey_1 <- read_rds(file.path(examples_dir, survey_files[1]))

## ----metadatacreate-----------------------------------------------------------
metadata_create(survey_1) %>% head()

## ----surveypaths--------------------------------------------------------------
survey_paths <- file.path(examples_dir, survey_files)

## ----readtolist---------------------------------------------------------------
example_surveys <- read_surveys(survey_paths, .f = "read_rds")

## ----processlist--------------------------------------------------------------
set.seed(2022)
metadata_create(survey_list = example_surveys) %>%
  sample_n(12)

## ----createmetadatasurveys----------------------------------------------------
example_metadata <- metadata_create(survey_paths = survey_paths, .f = "read_rds")

## ----printexample-------------------------------------------------------------
set.seed(2022)
example_metadata %>%
  sample_n(12)

## ----subsetmetadata-----------------------------------------------------------
library(dplyr)
subset_example_metadata <- example_metadata %>%
  filter(grepl("trust", .data$var_label_orig)) %>%
  filter(grepl("european_parliament", .data$var_label_orig)) %>%
  select(all_of(c("filename", "var_label_orig", "var_name_orig", "valid_labels", "na_labels", "class_orig")))

subset_example_metadata

## ----examplelabels------------------------------------------------------------
unlist(subset_example_metadata$valid_labels[1])

## ----examplenalabels2---------------------------------------------------------
unlist(subset_example_metadata$valid_labels[2])
unlist(subset_example_metadata$na_labels[2])

