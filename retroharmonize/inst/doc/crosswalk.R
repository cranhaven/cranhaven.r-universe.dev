## ----knitrsetup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(retroharmonize)
library(dplyr)

## ----surveyfiles--------------------------------------------------------------
examples_dir <- system.file("examples", package = "retroharmonize")
survey_files <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
survey_files

## ----surveypaths--------------------------------------------------------------
survey_paths <- file.path(examples_dir, survey_files)

## ----examplemetadata----------------------------------------------------------
example_metadata <- metadata_create(survey_paths = survey_paths)

## ----subsetmetadata-----------------------------------------------------------
subset_example_metadata <- example_metadata %>%
  filter(grepl("^unique_identifier_in|trust|country_code", var_label_orig)) %>%
  filter(grepl(
    "^unique_identifier_in|european_parliament|country_code",
    var_label_orig
  )) %>%
  filter(var_name_orig != "uniqid")

subset_example_metadata

## ----ct1----------------------------------------------------------------------
ct <- crosswalk_table_create(subset_example_metadata)
ct

## ----varnames-----------------------------------------------------------------
ct %>%
  mutate(var_name_target = case_when(
    var_name_orig == "rowid" ~ .data$var_name_orig,
    var_name_orig == "isocntry" ~ "geo",
    TRUE ~ "trust_ep"
  )) %>%
  distinct(across(all_of(c("filename", "var_name_orig", "var_name_target"))))

## ----vallabels----------------------------------------------------------------
ct %>%
  mutate(val_numeric_target = case_when(
    val_numeric_orig == 1 ~ .data$val_numeric_orig,
    val_numeric_orig == 2 ~ 0,
    TRUE ~ 99999
  )) %>%
  mutate(val_label_target = case_when(
    val_numeric_orig == 1 ~ "trust",
    val_numeric_orig == 2 ~ "distrust",
    TRUE ~ "declined"
  )) %>%
  distinct(across(all_of(c(
    "filename",
    "val_numeric_orig", "val_numeric_target",
    "val_label_orig", "val_label_target"
  ))))

## ----nalabels-----------------------------------------------------------------
ct %>%
  mutate(na_numeric_target = case_when(
    na_numeric_orig == 3 ~ 99999,
    TRUE ~ NA_real_
  )) %>%
  mutate(na_label_target = case_when(
    na_numeric_target == 99999 ~ "declined",
    TRUE ~ NA_character_
  )) %>%
  distinct(across(all_of(c(
    "filename", "val_numeric_orig",
    "na_numeric_orig", "na_numeric_target",
    "na_label_orig", "na_label_target"
  ))))

## ----crosswalktarget----------------------------------------------------------
example_crosswalk_table <- ct %>%
  mutate(var_name_target = case_when(
    var_name_orig == "rowid" ~ .data$var_name_orig,
    var_name_orig == "isocntry" ~ "geo",
    TRUE ~ "trust_ep"
  )) %>%
  mutate(val_numeric_target = case_when(
    val_numeric_orig == 1 ~ .data$val_numeric_orig,
    val_numeric_orig == 2 ~ 0,
    TRUE ~ 99999
  )) %>%
  mutate(val_label_target = case_when(
    val_numeric_orig == 1 ~ "trust",
    val_numeric_orig == 2 ~ "distrust",
    TRUE ~ "declined"
  )) %>%
  mutate(na_numeric_target = case_when(
    na_numeric_orig == 3 ~ 99999,
    TRUE ~ NA_real_
  )) %>%
  mutate(na_label_target = case_when(
    na_numeric_target == 99999 ~ "declined",
    TRUE ~ NA_character_
  ))

example_crosswalk_table

## ----readtomemory-------------------------------------------------------------
example_surveys <- read_surveys(survey_paths, .f = "read_rds")

## ----subsetsave1--------------------------------------------------------------
subset_survey_list_1 <- subset_surveys(
  survey_list = example_surveys,
  subset_vars = c("rowid", "isocntry", "qa10_1", "qa14_1"),
  subset_name = "subset_example"
)

## ----subsetsave11-------------------------------------------------------------
vapply(subset_survey_list_1, function(x) attr(x, "id"), character(1))

## ----subsetsave13-------------------------------------------------------------
head(subset_survey_list_1[[1]])

## ----subsetsave14-------------------------------------------------------------
lapply(subset_survey_list_1, names)

## ----subsetsave21-------------------------------------------------------------
subset_survey_list_2 <- subset_surveys(
  crosswalk_table = example_crosswalk_table,
  survey_list = example_surveys,
  subset_name = "trust_ep"
)

## ----subsetsave22-------------------------------------------------------------
vapply(subset_survey_list_2, function(x) attr(x, "id"), character(1))

## ----subsetsave23-------------------------------------------------------------
head(subset_survey_list_2[[1]])

## ----subsetsave24-------------------------------------------------------------
lapply(subset_survey_list_2, names)

## ----subsetsave---------------------------------------------------------------
subset_surveys(
  survey_list = example_surveys,
  crosswalk_table = example_crosswalk_table,
  subset_name = "trust_ep",
  import_path = examples_dir,
  export_path = tempdir()
)

## ----reread-------------------------------------------------------------------
readRDS(file.path(tempdir(), "ZA5913_trust_ep.rds")) %>%
  head()

