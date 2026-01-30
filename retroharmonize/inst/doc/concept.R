## ----include = FALSE----------------------------------------------------------
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

## ----readtolist---------------------------------------------------------------
example_surveys <- read_surveys(survey_paths, .f = "read_rds")

## ----createmetadatasurveys----------------------------------------------------
# not evaluated
example_metadata <- metadata_create(survey_paths = survey_paths, .f = "read_rds")

## ----processlist--------------------------------------------------------------
set.seed(2022)
metadata_create(survey_list = example_surveys) %>%
  dplyr::sample_n(12)

## -----------------------------------------------------------------------------
set.seed(12)
example_metadata %>%
  select(
    Filename = .data$filename,
    Name = .data$var_name_orig,
    Label = .data$var_label_orig,
    Type = .data$class_orig,
    Format = .data$labels
  ) %>%
  mutate(
    Valid = NA_real_,
    Invalid = NA_real_,
    Question = NA_character_
  ) %>%
  sample_n(12)

## -----------------------------------------------------------------------------
set.seed(12)
my_codebook <- create_codebook(
  survey = read_rds(
    system.file("examples", "ZA7576.rds",
      package = "retroharmonize"
    )
  )
)

sample_n(my_codebook, 12)

## -----------------------------------------------------------------------------
metadata <- metadata_create(example_surveys)
metadata$var_name_suggested <- label_normalize(metadata$var_name)
metadata$var_name_suggested[metadata$label_orig == "age_education"] <- "age_education"

harmonized_example_surveys <- harmonize_var_names(
  survey_list = example_surveys,
  metadata = metadata
)

lapply(harmonized_example_surveys, names)

## ----results='asis', message=FALSE--------------------------------------------
data.frame(
  Type = rep("discrete", 3),
  Format = c("numeric-1.0", "numeric-2.0", "numeric-6.0"),
  r_type = rep("integer", 3),
  range = c("0..9", "10..99", "100000..999999")
) %>% knitr::kable()

## -----------------------------------------------------------------------------
as.integer(1982)
as.character(as.integer(1982))

## ----results='asis', message=FALSE--------------------------------------------
data.frame(
  Type = rep("discrete", 3),
  Format = c("numeric-1.0", "numeric-2.0", "numeric-6.0"),
  r_type = rep("declared", 3),
  range = c("Male|Female|DK", "10..99", "100000..999999")
) %>% knitr::kable()

