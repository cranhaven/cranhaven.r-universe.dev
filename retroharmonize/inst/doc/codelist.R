## ----setupvignette, include = FALSE-------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(retroharmonize)
library(statcodelists)
library(dplyr)
library(knitr)

## ----createcodebook-----------------------------------------------------------
set.seed(12)
my_codebook <- create_codebook(
  survey = read_rds(
    system.file("examples", "ZA7576.rds",
      package = "retroharmonize"
    )
  )
)

sample_n(my_codebook, 12) %>%
  select(.data$filename,
    # Rename variables to DDI Codebook names
    Name = .data$var_name_orig,
    Label = .data$var_label_orig,
    .data$val_code_orig, .data$val_label_orig
  ) %>%
  kable()

## ----statcodelists------------------------------------------------------------
library(statcodelists)
CL_SEX

## ----sexexample, message=FALSE------------------------------------------------
data.frame(
  Value = c(1, 2),
  Label = c("Male", "Female")
)

## ----marriagestatusexample----------------------------------------------------
data.frame(
  Value = c(1, 2, 11),
  Label = c(
    "(Re-)Married: without children", 
    "(Re-)Married: children this marriage",
    "Divorced/Separated: without children"
  ),
  Normalized = c(
    "Married or Remarried without children",
    "Married or Remarried with children this marriage",
    "Divorced or Separated without children"
  )
) %>% kable()

