## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(datacutr)
library(admiraldev)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(rlang)

source_data <- list(
  ds = datacutr_ds, dm = datacutr_dm, ae = datacutr_ae, sc = datacutr_sc,
  lb = datacutr_lb, fa = datacutr_fa, ts = datacutr_ts
)

## ----message=FALSE, warning=FALSE---------------------------------------------
dcut <- create_dcut(
  dataset_ds = source_data$ds,
  ds_date_var = DSSTDTC,
  filter = DSDECOD == "RANDOMIZATION",
  cut_date = "2022-06-04",
  cut_description = "Clinical Cutoff Date"
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  dcut,
  display_vars = exprs(USUBJID, DCUTDTC, DCUTDTM, DCUTDESC)
)

## ----message=FALSE, warning=FALSE---------------------------------------------
source_data$fa <- source_data$fa %>%
  mutate(DCUT_TEMP_FAXDTC = case_when(
    FASTDTC != "" ~ FASTDTC,
    FADTC != "" ~ FADTC,
    TRUE ~ as.character(NA)
  ))

## ----message=FALSE, warning=FALSE---------------------------------------------
cut_data <- process_cut(
  source_sdtm_data = source_data,
  patient_cut_v = c("sc", "ds"),
  date_cut_m = rbind(
    c("ae", "AESTDTC"),
    c("lb", "LBDTC"),
    c("fa", "DCUT_TEMP_FAXDTC")
  ),
  no_cut_v = c("ts"),
  dataset_cut = dcut,
  cut_var = DCUTDTM,
  special_dm = TRUE
)

