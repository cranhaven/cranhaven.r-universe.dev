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
  cut_date = as.character(lubridate::today()),
  cut_description = "Week 24 Cut"
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  dcut,
  display_vars = exprs(USUBJID, DCUTDTC, DCUTDTM, DCUTDESC)
)

## ----message=FALSE, warning=FALSE---------------------------------------------
sv <- tibble::tribble(
  ~USUBJID, ~VISIT, ~SVSTDTC,
  "AB12345-001", "WEEK24", "2022-06-01",
  "AB12345-002", "WEEK24", "2022-06-30",
  "AB12345-003", "WEEK24", "2022-07-01",
  "AB12345-004", "WEEK24", "2022-05-04",
)

dcut <- dcut %>%
  left_join(sv %>%
    filter(VISIT == "WEEK24") %>%
    select(USUBJID, SVSTDTC)) %>%
  mutate(DCUTDTC = as.character(ifelse(!is.na(SVSTDTC), SVSTDTC, as.character(DCUTDTC)))) %>%
  impute_dcutdtc(dsin = ., varin = DCUTDTC, varout = DCUTDTM)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  dcut,
  display_vars = exprs(USUBJID, DCUTDTC, DCUTDTM, DCUTDESC)
)

