## ----setup, include=FALSE-----------------------------------------------------
library(surveycore)
knitr::opts_chunk$set(
  comment = "#>",
  eval = requireNamespace("surveytidy", quietly = TRUE)
)

## ----as_survey----------------------------------------------------------------
gss_svy <- as_survey(
  gss_2024,
  # the cluster ids
  ids = vpsu,
  # the strata
  strata = vstrat,
  # the weights
  weights = wtssps,
  nest = TRUE
)

gss_svy

## ----srs----------------------------------------------------------------------
ca_api_2000_svy <- as_survey(
  ca_api_2000,
  weights = pw,
  fpc = fpc # reduces SEs
)

ca_api_2000_svy

## ----replicate----------------------------------------------------------------
pew_jewish_svy <- as_survey_replicate(
  pew_jewish_2020,
  weights = extweight,
  repweights = extweight1:extweight100,
  type = "JK2"
)

pew_jewish_svy

## ----calibrated---------------------------------------------------------------
ns_wave1_svy <- as_survey_nonprob(ns_wave1, weights = weight)

ns_wave1_svy

## ----nwtco, eval=requireNamespace("survival", quietly=TRUE)-------------------
nwtco <- survival::nwtco

# in.subcohort is stored as 0/1 — must be logical for as_survey_twophase()
nwtco$in.subcohort <- as.logical(nwtco$in.subcohort)

# Phase 1: all 4,028 enrolled patients (each patient is their own unit)
phase1 <- as_survey(nwtco, ids = seqno)

# Phase 2: subcohort, with Phase 2 sampling stratified by relapse status
nwtco_svy <- as_survey_twophase(
  phase1,
  strata2 = rel, # Phase 2 strata: cases (rel=1) vs. non-cases (rel=0)
  subset = in.subcohort, # Logical column: TRUE = selected into Phase 2
  method = "full"
)

nwtco_svy

## ----freqs-basic--------------------------------------------------------------
get_freqs(ns_wave1_svy, consider_trump)

## ----freqs-multi--------------------------------------------------------------
get_freqs(ns_wave1_svy, c(news_sources_facebook:news_sources_other))

## ----freqs-rename-------------------------------------------------------------
ns_wave1_svy |>
  get_freqs(
    c(news_sources_facebook:news_sources_other),
    names_to = "news_source",
    values_to = "choice"
  )

## ----means-basic--------------------------------------------------------------
# Average favorability towards Biden
ns_wave1_svy |>
  # remove those who said "Not sure" (coded as 999)
  surveytidy::filter_out(cand_favorability_biden == 999) |>
  get_means(cand_favorability_biden)

## ----totals-pew---------------------------------------------------------------
pew_jewish_svy |>
  # only include jews by religion and jews of no religion to match Pew's report
  surveytidy::filter(jewishcat %in% c(1:2)) |>
  get_totals()

## ----totals-ns----------------------------------------------------------------
get_totals(gss_svy)

## ----totals-x-----------------------------------------------------------------
get_totals(ca_api_2000_svy, x = enroll)

## ----totals-group-------------------------------------------------------------
pew_jewish_svy |>
  # only include jews by religion and jews of no religion to match Pew's report
  surveytidy::filter(jewishcat %in% c(1:2)) |>
  get_totals(group = age4cat)

## ----corr-basic---------------------------------------------------------------
ns_wave1_clean_svy <- ns_wave1_svy |>
  surveytidy::drop_na(
    cand_favorability_trump,
    cand_favorability_biden
  ) |>
  surveytidy::filter_out(
    cand_favorability_trump == 999,
    cand_favorability_biden == 999
  )

get_corr(
  ns_wave1_clean_svy,
  c(cand_favorability_trump, cand_favorability_biden)
)

## ----corr-multi---------------------------------------------------------------
fav_vars <- c(
  "cand_favorability_trump",
  "cand_favorability_biden",
  "cand_favorability_harris",
  "cand_favorability_sanders",
  "cand_favorability_warren",
  "cand_favorability_buttigieg",
  "cand_favorability_pence"
)

ns_wave1_multi <- ns_wave1_clean_svy |>
  # remove NAs from all variables of interest
  surveytidy::drop_na(tidyselect::all_of(fav_vars)) |>
  # remove those who said "not sure" to any variable of interest
  surveytidy::filter_out(
    dplyr::if_any(
      tidyselect::all_of(fav_vars),
      \(x) x == 999
    )
  )

get_corr(
  ns_wave1_multi,
  c(cand_favorability_trump:cand_favorability_pence)
)

## ----corr-wide----------------------------------------------------------------
get_corr(
  ns_wave1_multi,
  c(cand_favorability_trump:cand_favorability_pence),
  format = "wide"
)

## ----ratios-basic-------------------------------------------------------------
get_ratios(
  ns_wave1_multi,
  numerator = cand_favorability_trump,
  denominator = cand_favorability_biden
)

## ----quantiles-basic----------------------------------------------------------
# Quartiles and median of age (default probs = c(0.25, 0.5, 0.75))
get_quantiles(ns_wave1_svy, age)

## ----diffs-basic--------------------------------------------------------------
ns_wave1_svy |>
  surveytidy::filter_out(cand_favorability_biden == 999) |>
  get_diffs(cand_favorability_biden, treats = pid3)

## ----diffs-pct----------------------------------------------------------------
ns_wave1_svy |>
  surveytidy::filter_out(cand_favorability_biden == 999) |>
  get_diffs(
    cand_favorability_biden,
    treats = pid3,
    show_pct_change = TRUE
  )

## ----t-test-basic-------------------------------------------------------------
get_t_test(gss_svy, hrs1, by = sex)

## ----pairwise-basic-----------------------------------------------------------
get_pairwise(ns_wave1_svy, age, by = pid3)

## ----variance-basic-----------------------------------------------------------
get_variance(ns_wave1_svy, age)

## ----group-means--------------------------------------------------------------
get_freqs(ns_wave1_svy, consider_trump, group = pid3)

## ----variance-options---------------------------------------------------------
get_means(
  ns_wave1_svy,
  age,
  variance = c("se", "ci", "moe"),
  conf_level = 0.9
)

## ----n-weighted---------------------------------------------------------------
get_freqs(pew_jewish_svy, age4cat, n_weighted = TRUE)

## ----glm-fit------------------------------------------------------------------
fit <- gss_svy |>
  # convert race to a factor so one variable is a factor
  surveytidy::mutate(
    race_f = surveytidy::make_factor(race)
  ) |>
  survey_glm(hrs1 ~ sex + degree + age + race_f)

fit

## ----glm-clean----------------------------------------------------------------
clean(fit)

