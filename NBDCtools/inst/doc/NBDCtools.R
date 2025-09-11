## ----include = FALSE----------------------------------------------------------
Sys.setenv("_R_CHECK_CRAN_INCOMING_" = "true") # always build vignette like on CRAN
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NBDCtools)

## -----------------------------------------------------------------------------
dir_abcd <- system.file("extdata", "phenotype", package = "NBDCtools")
list.files(dir_abcd)

## -----------------------------------------------------------------------------
vars <- c(
  "ab_g_dyn__visit_type", 
  "ab_g_dyn__cohort_grade", 
  "ab_g_dyn__visit__day1_dt", 
  "ab_g_stc__gen_pc__01", 
  "ab_g_dyn__visit_age", 
  "ab_g_dyn__visit_days", 
  "ab_g_dyn__visit_dtt", 
  "mr_y_qc__raw__dmri__r01__series_t"
)
create_dataset(
  dir_data = dir_abcd,
  study = "abcd",
  vars = vars
)

## -----------------------------------------------------------------------------
create_dataset(
  dir_data = dir_abcd,
  study = "abcd",
  vars = c(
    "mr_y_qc__raw__dmri__r01__series_t"
  ),
  vars_add = c(
    "ab_g_dyn__visit_type",
    "ab_g_dyn__cohort_grade", 
    "ab_g_dyn__visit__day1_dt",
    "ab_g_stc__gen_pc__01", 
    "ab_g_dyn__visit_age",
    "ab_g_dyn__visit_days", 
    "ab_g_dyn__visit_dtt"
  )
)

## -----------------------------------------------------------------------------
create_dataset(
  dir_data = dir_abcd,
  study = "abcd",
  vars = vars,
  value_to_label = TRUE,
  value_to_na = TRUE,
  time_to_hms = TRUE
)

## ----eval=FALSE---------------------------------------------------------------
#  create_dataset(
#    dir_data = dir_abcd,
#    study = "abcd",
#    vars = vars,
#    bind_shadow = TRUE
#  )

## ----eval=FALSE---------------------------------------------------------------
#  create_dataset(
#    dir_data = dir_abcd,
#    study = "abcd",
#    vars = vars,
#    bind_shadow = TRUE,
#    naniar_shadow = TRUE
#  )

## -----------------------------------------------------------------------------
create_dataset(
  dir_data = dir_abcd,
  study = "abcd",
  vars = vars,
  value_to_na = TRUE,
  missing_codes = c("1", "2")
)

## -----------------------------------------------------------------------------
create_dataset(
  dir_data = dir_abcd,
  study = "abcd",
  vars = vars,
  value_to_na = TRUE,
  my_arg = "some_value" # this argument will be ignored
)

