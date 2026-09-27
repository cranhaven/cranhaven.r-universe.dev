## ----setup, include=FALSE-----------------------------------------------------
library(surveycore)
knitr::opts_chunk$set(comment = "#>")

has_survey     <- requireNamespace("survey",     quietly = TRUE)
has_srvyr      <- requireNamespace("srvyr",      quietly = TRUE)

if (has_survey) {
  library(survey)
  data(api) # loads apisrs, apistrat, apiclus1
}
if (has_srvyr) suppressMessages(library(srvyr))

## ----srs-survey, eval=has_survey----------------------------------------------
srs_sv <- svydesign(ids = ~1, fpc = ~fpc, weights = ~pw, data = apisrs)
srs_sv

## ----srs-srvyr, eval=has_survey && has_srvyr----------------------------------
srs_srvyr <- apisrs |> as_survey_design(ids = 1, fpc = fpc, weights = pw)
srs_srvyr

## ----srs-sc, eval=has_survey--------------------------------------------------
srs_sc <- surveycore::as_survey(apisrs, weights = pw, fpc = fpc)
srs_sc

## ----strat-survey, eval=has_survey--------------------------------------------
strat_sv <- svydesign(
  ids = ~1, strata = ~stype, weights = ~pw, fpc = ~fpc, data = apistrat
)
strat_sv

## ----strat-srvyr, eval=has_survey && has_srvyr--------------------------------
strat_srvyr <- apistrat |>
  as_survey_design(strata = stype, weights = pw, fpc = fpc)
strat_srvyr

## ----strat-sc, eval=has_survey------------------------------------------------
strat_sc <- surveycore::as_survey(apistrat, strata = stype, weights = pw, fpc = fpc)
strat_sc

## ----clus-survey, eval=has_survey---------------------------------------------
clus_sv <- svydesign(ids = ~dnum, fpc = ~fpc, weights = ~pw, data = apiclus1)
clus_sv

## ----clus-srvyr, eval=has_survey && has_srvyr---------------------------------
clus_srvyr <- apiclus1 |>
  as_survey_design(ids = dnum, fpc = fpc, weights = pw)
clus_srvyr

## ----clus-sc, eval=has_survey-------------------------------------------------
clus_sc <- surveycore::as_survey(apiclus1, ids = dnum, fpc = fpc, weights = pw)
clus_sc

## ----repwt-acs-survey, eval=has_survey----------------------------------------
acs_sv <- svrepdesign(
  data             = acs_pums_wy,
  weights          = ~pwgtp,
  repweights       = "pwgtp[0-9]+",   # regex string
  type             = "successive-difference",
  combined.weights = TRUE
)
acs_sv

## ----repwt-acs-srvyr, eval=has_survey && has_srvyr----------------------------
acs_srvyr <- acs_pums_wy |>
  as_survey_rep(
    weights          = pwgtp,
    repweights       = matches("^pwgtp[0-9]+$"), # tidyselect
    type             = "successive-difference",
    combined_weights = TRUE
  )
acs_srvyr

## ----repwt-acs-sc-------------------------------------------------------------
acs_sc <- as_survey_replicate(
  acs_pums_wy,
  weights    = pwgtp,
  repweights = tidyselect::matches("^pwgtp[0-9]+$"), # tidyselect
  type       = "successive-difference"
)
acs_sc

## ----repwt-pew-sc-------------------------------------------------------------
pew_sc <- as_survey_replicate(
  pew_jewish_2020,
  weights    = extweight,
  repweights = extweight1:extweight100,
  type       = "JK1"
)
pew_sc

## ----calib-survey, eval=has_survey--------------------------------------------
# No way to signal this is calibrated or non-probability
ns_sv <- svydesign(ids = ~1, weights = ~weight, data = ns_wave1)

## ----calib-srvyr, eval=has_survey && has_srvyr--------------------------------
ns_srvyr <- ns_wave1 |> as_survey_design(weights = weight)

## ----calib-sc-----------------------------------------------------------------
# as_survey_nonprob() makes the design type explicit
ns_sc <- as_survey_nonprob(ns_wave1, weights = weight)
ns_sc

## ----means-survey, eval=has_survey--------------------------------------------
svyby(~discrimination_blacks, ~pid3, ns_sv, svymean, na.rm = TRUE)

## ----means-srvyr, eval=has_survey && has_srvyr--------------------------------
ns_srvyr |>
  group_by(pid3) |>
  summarise(m = survey_mean(discrimination_blacks, vartype = "ci", na.rm = TRUE))

## ----means-sc-----------------------------------------------------------------
get_means(ns_sc, discrimination_blacks, group = pid3)

## ----freqs-survey, eval=has_survey--------------------------------------------
svymean(~factor(consider_trump), ns_sv, na.rm = TRUE)

## ----freqs-srvyr, eval=has_survey && has_srvyr--------------------------------
ns_srvyr |>
  group_by(consider_trump) |>
  summarise(pct = survey_mean(na.rm = TRUE))

## ----freqs-sc-----------------------------------------------------------------
get_freqs(ns_sc, consider_trump)

## ----totals-survey, eval=has_survey-------------------------------------------
sum(weights(ns_sv))                         # estimated population N
svytotal(~age, ns_sv, na.rm = TRUE)         # total of a continuous variable

## ----totals-srvyr, eval=has_survey && has_srvyr-------------------------------
ns_srvyr |> summarise(n_pop = survey_total(1))       # estimated N
ns_srvyr |> summarise(age_total = survey_total(age, na.rm = TRUE))

## ----totals-sc----------------------------------------------------------------
get_totals(ns_sc)           # estimated N (no x argument)
get_totals(ns_sc, age)      # total of a continuous variable

## ----totals-pew---------------------------------------------------------------
get_totals(pew_sc)

## ----quantiles-survey, eval=has_survey----------------------------------------
svyquantile(~age, ns_sv, quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE)

## ----quantiles-srvyr, eval=has_survey && has_srvyr----------------------------
ns_srvyr |>
  summarise(q = survey_quantile(age, c(0.25, 0.5, 0.75), na.rm = TRUE))

## ----quantiles-sc-------------------------------------------------------------
get_quantiles(ns_sc, age)

## ----ratios-survey, eval=has_survey-------------------------------------------
svyratio(~api00, ~api99, srs_sv)

## ----ratios-srvyr, eval=has_survey && has_srvyr-------------------------------
srs_srvyr |> summarise(ratio = survey_ratio(api00, api99))

## ----ratios-sc, eval=has_survey-----------------------------------------------
get_ratios(srs_sc, numerator = api00, denominator = api99)

## ----corr-setup---------------------------------------------------------------
# Pre-filter non-substantive responses before creating the design
ns_corr <- ns_wave1[
  !is.na(ns_wave1$cand_favorability_trump) &
    ns_wave1$cand_favorability_trump != 999 &
    !is.na(ns_wave1$cand_favorability_biden) &
    ns_wave1$cand_favorability_biden != 999,
]
ns_corr_sc <- as_survey_nonprob(ns_corr, weights = weight)

## ----corr-survey, eval=has_survey && requireNamespace("jtools", quietly = TRUE)----
ns_corr_sv <- svydesign(ids = ~1, weights = ~weight, data = ns_corr)
jtools::svycor(~cand_favorability_trump + cand_favorability_biden, ns_corr_sv)

## ----corr-sc------------------------------------------------------------------
get_corr(ns_corr_sc, c(cand_favorability_trump, cand_favorability_biden))

## ----uncertainty-survey, eval=has_survey--------------------------------------
m <- svymean(~age, ns_sv, na.rm = TRUE)
m                      # SE only in the estimate
confint(m)             # CI — separate call
cv(m)                  # CV — separate call
svymean(~age, ns_sv, deff = TRUE, na.rm = TRUE) # DEFF — different return structure

## ----uncertainty-srvyr, eval=has_survey && has_srvyr--------------------------
ns_srvyr |>
  summarise(
    m_se   = survey_mean(age, vartype = "se",   na.rm = TRUE),
    m_ci   = survey_mean(age, vartype = "ci",   na.rm = TRUE),
    m_cv   = survey_mean(age, vartype = "cv",   na.rm = TRUE),
    m_deff = survey_mean(age, deff = TRUE,      na.rm = TRUE)
  )

## ----uncertainty-sc-----------------------------------------------------------
get_means(ns_sc, age, variance = c("se", "ci", "cv", "deff"))

## ----uncertainty-null---------------------------------------------------------
get_means(ns_sc, age, variance = NULL)

## ----labels-survey, eval=has_survey-------------------------------------------
# pid3 values: 1, 2, 3, 4 — the reader must consult the codebook
svyby(~discrimination_blacks, ~pid3, ns_sv, svymean, na.rm = TRUE)

## ----labels-sc----------------------------------------------------------------
get_means(ns_sc, discrimination_blacks, group = pid3)

## ----labels-optout------------------------------------------------------------
get_means(ns_sc, discrimination_blacks, group = pid3, label_values = FALSE)

## ----multi-survey, eval=has_survey--------------------------------------------
news_vars <- c(
  "news_sources_facebook", "news_sources_cnn", "news_sources_fox",
  "news_sources_npr", "news_sources_new_york_times"
)
results_sv <- lapply(news_vars, function(v) {
  f <- as.formula(paste0("~", v))
  svymean(f, ns_sv, na.rm = TRUE)
})
# Results are a list — user must bind rows and add a name column manually
do.call(rbind, lapply(seq_along(results_sv), function(i) {
  data.frame(name = news_vars[[i]], coef(results_sv[[i]]))
}))

## ----multi-sc-----------------------------------------------------------------
get_freqs(
  ns_sc,
  c(news_sources_facebook:news_sources_other)
)

## ----min-cell-----------------------------------------------------------------
# Construct a design with deliberately small cells
small_df <- data.frame(
  group = rep(c("A", "B", "C"), c(8, 15, 200)),
  x     = rnorm(223),
  w     = 1
)
small_svy <- surveycore::as_survey(small_df, weights = w)

get_means(small_svy, x, group = group)

## ----min-cell-suppress, eval=FALSE--------------------------------------------
# get_means(small_svy, x, group = group, min_cell_n = 0L)

## ----n-weighted-survey, eval=has_survey---------------------------------------
# Proportions by group (unweighted n not shown in output)
svyby(~factor(consider_trump), ~pid3, ns_sv, svymean, na.rm = TRUE)
# Estimated weighted N per group — requires a separate call
svyby(~as.numeric(!is.na(consider_trump)), ~pid3, ns_sv, svytotal, na.rm = TRUE)

## ----n-weighted-sc------------------------------------------------------------
get_freqs(ns_sc, consider_trump, group = pid3, n_weighted = TRUE)

## ----meta---------------------------------------------------------------------
result <- get_means(ns_sc, discrimination_blacks, group = pid3)

# Variable label for the focal variable
attr(result, ".meta")$x$discrimination_blacks$variable_label

# Value labels for the grouping variable
attr(result, ".meta")$group$pid3$value_labels

