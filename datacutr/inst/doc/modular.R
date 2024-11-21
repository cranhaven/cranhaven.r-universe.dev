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

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  source_data$fa,
  display_vars = exprs(USUBJID, FASTDTC, FADTC, DCUT_TEMP_FAXDTC)
)

## ----message=FALSE, warning=FALSE---------------------------------------------
patient_cut_list <- c("sc", "ds")

date_cut_list <- rbind(
  c("ae", "AESTDTC"),
  c("lb", "LBDTC"),
  c("fa", "DCUT_TEMP_FAXDTC")
)

no_cut_list <- list(ts = source_data$ts)

## ----message=FALSE, warning=FALSE---------------------------------------------
patient_cut_data <- lapply(
  source_data[patient_cut_list], pt_cut,
  dataset_cut = dcut
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  patient_cut_data$sc,
  display_vars = exprs(USUBJID, SCORRES, DCUT_TEMP_REMOVE)
)

## ----message=FALSE, warning=FALSE---------------------------------------------
date_cut_data <- pmap(
  .l = list(
    dataset_sdtm = source_data[date_cut_list[, 1]],
    sdtm_date_var = syms(date_cut_list[, 2])
  ),
  .f = date_cut,
  dataset_cut = dcut,
  cut_var = DCUTDTM
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  date_cut_data$ae,
  display_vars = exprs(
    USUBJID,
    AETERM,
    AESTDTC,
    DCUT_TEMP_SDTM_DATE,
    DCUT_TEMP_DCUTDTM,
    DCUT_TEMP_REMOVE
  )
)

## ----message=FALSE, warning=FALSE---------------------------------------------
dm_cut <- special_dm_cut(
  dataset_dm = source_data$dm,
  dataset_cut = dcut,
  cut_var = DCUTDTM
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  dm_cut,
  display_vars = exprs(
    USUBJID,
    DTHFL,
    DTHDTC,
    DCUT_TEMP_REMOVE,
    DCUT_TEMP_DTHDT,
    DCUT_TEMP_DCUTDTM,
    DCUT_TEMP_DTHCHANGE
  )
)

## ----message=FALSE, warning=FALSE---------------------------------------------
cut_data <- purrr::map(
  c(patient_cut_data, date_cut_data, list(dm = dm_cut)),
  apply_cut,
  dcutvar = DCUT_TEMP_REMOVE,
  dthchangevar = DCUT_TEMP_DTHCHANGE
)

## ----message=FALSE, warning=FALSE---------------------------------------------
final_data <- c(cut_data, no_cut_list, list(dcut = dcut))

