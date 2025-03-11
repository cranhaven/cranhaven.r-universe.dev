## ----setup, include = FALSE---------------------------------------------------
suggested_dependent_pkgs <- c("rstan", "V8")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = all(vapply(
    suggested_dependent_pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  ))
)

## ----eval=FALSE---------------------------------------------------------------
# vignette(topic = "quickstart", package = "rbmi")

## -----------------------------------------------------------------------------
library(tern.rbmi)
library(dplyr)

## -----------------------------------------------------------------------------
data <- antidepressant_data
levels(data$THERAPY) <- c("PLACEBO", "DRUG") # This is important! The order defines the computation order later

missing_var <- "CHANGE"
vars <- list(
  id = "PATIENT",
  visit = "VISIT",
  expand_vars = c("BASVAL", "THERAPY"),
  group = "THERAPY"
)
covariates <- list(
  draws = c("BASVAL*VISIT", "THERAPY*VISIT"),
  analyse = c("BASVAL")
)

data <- data %>%
  dplyr::select(PATIENT, THERAPY, VISIT, BASVAL, THERAPY, CHANGE) %>%
  dplyr::mutate(dplyr::across(.cols = vars$id, ~ as.factor(.x))) %>%
  dplyr::arrange(dplyr::across(.cols = c(vars$id, vars$visit)))

## -----------------------------------------------------------------------------
# Use expand_locf to add rows corresponding to visits with missing outcomes to the dataset
data_full <- do.call(
  expand_locf,
  args = list(
    data = data,
    vars = c(vars$expand_vars, vars$group),
    group = vars$id,
    order = c(vars$id, vars$visit)
  ) %>%
    append(lapply(data[c(vars$id, vars$visit)], levels))
)

data_full <- data_full %>%
  dplyr::group_by(dplyr::across(vars$id)) %>%
  dplyr::mutate(!!vars$group := Filter(Negate(is.na), .data[[vars$group]])[1])

# there are duplicates - use first value
data_full <- data_full %>%
  dplyr::group_by(dplyr::across(c(vars$id, vars$group, vars$visit))) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()
# need to have a single ID column
data_full <- data_full %>%
  tidyr::unite("TMP_ID", dplyr::all_of(vars$id), sep = "_#_", remove = FALSE) %>%
  dplyr::mutate(TMP_ID = as.factor(TMP_ID))

## -----------------------------------------------------------------------------
data_ice <- data_full %>%
  dplyr::arrange(dplyr::across(.cols = c("TMP_ID", vars$visit))) %>%
  dplyr::filter(is.na(.data[[missing_var]])) %>%
  dplyr::group_by(TMP_ID) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(all_of(c("TMP_ID", vars$visit))) %>%
  dplyr::mutate(strategy = "MAR")

## -----------------------------------------------------------------------------
debug_mode <- FALSE

draws_vars <- rbmi::set_vars(
  outcome = missing_var,
  visit = vars$visit,
  group = vars$group,
  covariates = covariates$draws
)
draws_vars$subjid <- "TMP_ID"

## -----------------------------------------------------------------------------
draws_method <- rbmi::method_bayes()

draws_obj <- rbmi::draws(
  data = data_full,
  data_ice = data_ice,
  vars = draws_vars,
  method = draws_method
)

## -----------------------------------------------------------------------------
impute_references <- c("DRUG" = "PLACEBO", "PLACEBO" = "PLACEBO")

impute_obj <- rbmi::impute(
  draws_obj,
  references = impute_references
)

## -----------------------------------------------------------------------------
# Define analysis model
analyse_fun <- ancova

ref_levels <- levels(impute_obj$data$group[[1]])
names(ref_levels) <- c("ref", "alt")

analyse_obj <- rbmi::analyse(
  imputations = impute_obj,
  fun = analyse_fun,
  vars = rbmi::set_vars(
    subjid = "TMP_ID",
    outcome = missing_var,
    visit = vars$visit,
    group = vars$group,
    covariates = covariates$analyse
  )
)

## -----------------------------------------------------------------------------
pool_obj <- rbmi::pool(
  results = analyse_obj,
  conf.level = 0.95,
  alternative = c("two.sided", "less", "greater"),
  type = c("percentile", "normal")
)

## -----------------------------------------------------------------------------
library(broom)

df <- tidy(pool_obj)
df

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by("group", ref_group = levels(df$group)[1]) %>%
  split_rows_by("visit", split_label = "Visit", label_pos = "topleft") %>%
  summarize_rbmi() %>%
  build_table(df)

