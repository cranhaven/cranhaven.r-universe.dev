\name{comparison_methods}
\alias{comparison_methods}
\alias{Summary_stat}
\alias{LMM}
\alias{Linear_mixed_model}
\alias{Lin_Ying}
\alias{Liang}
\alias{Joint_modeling_visiting_and_longitudinal_Liang}
\alias{Pairwise_likelihood}
\alias{JMVL_G}
\alias{IIRR}
\alias{Inverse_intensity_rate_ratio_weighting}
\alias{IIRR_balancing}
\alias{Inverse_intensity_rate_ratio_balancing}
\alias{Multiple_imputation_IP}
\alias{Linear_increment_IP}
\alias{EHRJoint}
\title{Named Wrappers for the Comparison Methods}
\description{
  Convenience functions that fit one of the comparison methods by name.
  Each wrapper selects a method key from its own arguments and then calls
  \code{\link{fit_method}(method, data, ...)}, so every result is a
  \code{"cimehr_fit"} object with the same structure as a direct
  \code{fit_method} call.
}
\usage{
Summary_stat(data, stat = c("mean", "median", "min", "max"), ...)

LMM(data, ...)
Linear_mixed_model(data, ...)

Lin_Ying(data, process = c("marker", "visit"), ...)

Liang(data, process = c("marker", "visit"), ...)
Joint_modeling_visiting_and_longitudinal_Liang(data,
    process = c("marker", "visit"), ...)

Pairwise_likelihood(data, ...)

JMVL_G(data, process = c("marker", "visit"), ...)

IIRR(data, process = c("marker", "visit"), ...)
Inverse_intensity_rate_ratio_weighting(data, process = c("marker", "visit"), ...)

IIRR_balancing(data, process = c("marker", "visit"), ...)
Inverse_intensity_rate_ratio_balancing(data, process = c("marker", "visit"), ...)

Multiple_imputation_IP(data, ip_method = c("LMM", "Liang", "IIRR"), ...)

Linear_increment_IP(data, ip_method = c("LMM", "Liang", "IIRR"), ...)

EHRJoint(data, ...)
}
\arguments{
  \item{data}{A \code{data.frame} with one row per visit (see
    \code{\link{fit_method}} for the required layout).}
  \item{stat}{Subject-level summary of the recorded outcomes used by
    \code{Summary_stat}: \code{"mean"} (default), \code{"median"},
    \code{"min"} or \code{"max"}.}
  \item{process}{Event process that drives the method:
    \code{"marker"} uses the recorded-marker process (visits with
    \code{R = 1}); \code{"visit"} uses the clinic-visit process (all
    visits). The default is \code{"marker"}.}
  \item{ip_method}{Analysis applied after imputation (\code{Multiple_imputation_IP})
    or linear increments (\code{Linear_increment_IP}): \code{"LMM"}
    (default), \code{"Liang"} or \code{"IIRR"}.}
  \item{\dots}{Further arguments passed to \code{\link{fit_method}}
    (everything except \code{method} and \code{data}): the column names
    \code{id}, \code{time}, \code{outcome}, \code{obs_indicator},
    \code{censor_col} and \code{entry_col}; the covariate sets
    \code{visit_covs}, \code{long_covs} and \code{outcome_random_covs};
    \code{outcome_random_structure}, \code{outcome_random_intercept},
    \code{seed}, \code{controls}; and named method-specific controls,
    which \code{fit_method} merges into \code{controls}.}
}
\details{
  The wrappers map to method keys as follows (keys and labels as in
  \code{\link{available_comparison_methods}()}):
  \tabular{lll}{
    \strong{Wrapper} \tab \strong{Selector} \tab \strong{Method key (label)} \cr
    \code{Summary_stat} \tab \code{stat = "min"}, \code{"mean"},
      \code{"median"}, \code{"max"} \tab \code{summary_min},
      \code{summary_mean}, \code{summary_median}, \code{summary_max}
      (Summary-Min, -Mean, -Median, -Max) \cr
    \code{LMM}, \code{Linear_mixed_model} \tab -- \tab \code{lmm} (LMM) \cr
    \code{Lin_Ying} \tab \code{process = "marker"}, \code{"visit"} \tab
      \code{lin_ying_marker}, \code{lin_ying_visit} (Lin-Ying) \cr
    \code{Liang}, \code{Joint_modeling_visiting_and_longitudinal_Liang}
      \tab \code{process = "marker"}, \code{"visit"} \tab
      \code{liang_marker}, \code{liang_visit} (Liang) \cr
    \code{Pairwise_likelihood} \tab -- \tab \code{pairlik} (PairLik) \cr
    \code{JMVL_G} \tab \code{process = "marker"}, \code{"visit"} \tab
      \code{jmvl_g_marker}, \code{jmvl_g_visit} (JMVL-G) \cr
    \code{IIRR}, \code{Inverse_intensity_rate_ratio_weighting} \tab
      \code{process = "marker"}, \code{"visit"} \tab
      \code{iirr_weighting_marker}, \code{iirr_weighting_visit}
      (IIRR-Weighting) \cr
    \code{IIRR_balancing}, \code{Inverse_intensity_rate_ratio_balancing}
      \tab \code{process = "marker"}, \code{"visit"} \tab
      \code{iirr_balancing_marker}, \code{iirr_balancing_visit}
      (IIRR-Balancing) \cr
    \code{Multiple_imputation_IP} \tab \code{ip_method = "LMM"},
      \code{"Liang"}, \code{"IIRR"} \tab \code{mi_lmm}, \code{mi_liang},
      \code{mi_iirr} (MI-LMM, MI-Liang, MI-IIRR) \cr
    \code{Linear_increment_IP} \tab \code{ip_method = "LMM"},
      \code{"Liang"}, \code{"IIRR"} \tab \code{li_lmm}, \code{li_liang},
      \code{li_iirr} (LI-LMM, LI-Liang, LI-IIRR) \cr
    \code{EHRJoint} \tab -- \tab \code{ehrjoint} (EHRJoint; Du, Shi and
      Mukherjee) \cr
  }
  Paired names (for example \code{Liang} and
  \code{Joint_modeling_visiting_and_longitudinal_Liang}) are the same
  function. \code{Lin_Ying(data, process = "visit", ...)} is therefore
  equivalent to \code{fit_method("lin_ying_visit", data, ...)}.
  \code{oa_lmm} (OA-LMM) and \code{va_lmm} (VA-LMM) have no wrapper and are
  fitted with \code{fit_method}; the CIMEHR estimators (\code{cimehr},
  \code{cimehr_hl}, \code{cimehr_spline}, \code{cimehr_spline_hl}) have
  their own functions \code{\link{CIMEHR}}, \code{\link{CIMEHR_HL}},
  \code{\link{CIMEHR_spline}} and \code{\link{CIMEHR_spline_HL}}.

  At an exactly zero estimated visiting log-frailty variance, the four CIMEHR
  methods retain a standard normal visiting-only posterior and the original
  observation/outcome compensation formula. The check concerns the attainable
  compensation space, not separate identification of every loading and residual
  variance. There is no rejection based only on the number of observation
  random-effect columns or the absence of a covariate excluded from the outcome
  model. When the exclusion-relevance criterion of \code{\link{CIMEHR}} is
  not met, the zero-frailty estimation procedure includes a
  zero-compensation restriction (\eqn{c = 0}); this does not establish the
  absence of informative observation, and the estimates and SEs are
  interpreted under that restriction. The software does not infer an
  exclusion restriction from covariate names or use those names to determine
  interval availability. See \sQuote{Zero visiting frailty} in
  \code{\link{CIMEHR}}.

  Exact total-variance and compensation coordinates are used when supported by
  the original model. A scalar observation random intercept can retain product
  coefficients without separately estimating its shared loading. Saturated
  observation groups use group coordinates only when the original outcome
  random-effect design supports the same attainable compensation space. A
  nonlinear attainable set is not replaced by a larger unrestricted linear
  span. The compensation must also be separable from fixed outcome effects
  after profiling the baseline. Otherwise fitting stops with a
  \code{cimehr_zero_frailty_identification_error} and a specific reason, rather
  than returning estimates from an arbitrary latent decomposition. A locally
  unique loading decomposition up to common sign is not rejected solely
  because it uses the original loading coordinates. Numerical rank at a fitted
  covariance does not prove regularity at the true parameter.

  Default stacked covariance propagates the remaining identifiable visiting,
  observation, and outcome quantities, including cross-stage covariance and
  the estimated probabilities, compensation multipliers, and centering weights.
  This inference is conditional on the selected zero-frailty branch and
  variance face; it does not assert unconditional or selection-adjusted
  coverage. Individual latent parameters that cannot be separated are not
  reported as identified. The \code{zero_frailty_compensation} component and
  diagnostics record the compensation coordinates and identification scope.
  Numerical rank at a fitted value does not prove regularity at the true
  parameter.

  The covariate arguments of \code{fit_method} default to \code{NULL}
  (no covariates), so \code{visit_covs} (visit-process covariates) and
  \code{long_covs} (outcome-model covariates) should normally be supplied.
  \code{outcome_random_covs} gives the random-effect (or random-link)
  covariates for the methods that model them: \code{lmm},
  \code{liang_marker}, \code{liang_visit}, the multiple-imputation methods
  (\code{mi_lmm}, \code{mi_liang}, \code{mi_iirr}), \code{li_lmm},
  \code{li_liang} and \code{ehrjoint}.

  For \code{EHRJoint}, \code{obs_fixed_covs} specifies the recording-model
  covariates separately from \code{visit_covs} and \code{long_covs}. These
  predictors must be numeric and constant within subject. Its default is
  \code{visit_covs}, after any \code{controls} override; \code{character()}
  gives an intercept-only recording
  model. The stacked covariance uses this recording design and propagates
  recording-parameter uncertainty into the outcome estimates.

  Method-specific controls are given either in \code{controls = list(...)}
  or directly as named arguments; names that the selected method does not
  accept give an error. The available controls (with defaults) are:
  \describe{
    \item{\code{Lin_Ying}}{\code{optimizer_control = list(maxit = 1000L)},
      \code{inference = "stacked"}, \code{return_subject_scores = FALSE}.}
    \item{\code{Liang}}{\code{optimizer_control = list(maxit = 1000L)},
      \code{estimate_intercept = FALSE}, \code{baseline_formula = ~1},
      \code{inference = "stacked"}, \code{retain_influence = FALSE}.}
    \item{\code{Pairwise_likelihood}}{\code{time_formula = ~time} (a one-sided
      formula in the time column such as \code{~time + I(time^2)}, or
      \code{NULL}), \code{max_subject_pairs = 20000L},
      \code{optimizer_control = list(maxit = 2000L)}; all cross observation
      pairs of each sampled subject pair are used, and the pair sampling
      seed is \code{fit_method}'s \code{seed}.}
    \item{\code{JMVL_G}}{\code{include_time = TRUE}, \code{gh_order = 9L},
      \code{optimizer_control = list(maxit = 1500L, reltol = 1e-9)}.}
    \item{\code{IIRR}}{\code{include_time = TRUE},
      \code{optimizer_control = list(maxit = 1000L)},
      \code{return_subject_scores = FALSE}.}
    \item{\code{IIRR_balancing}}{\code{include_time = TRUE},
      \code{balance_time_interactions = TRUE}, \code{calibration_tol = 1e-8},
      \code{optimizer_control = list(maxit = 2000L)},
      \code{return_subject_scores = FALSE}.}
    \item{\code{Multiple_imputation_IP}}{\code{m = 5L} imputations,
      \code{maxit = 5L}, \code{keep_completed = FALSE}; for \code{"LMM"}
      also \code{lmm_method = c("REML", "ML")} and
      \code{optimizer = c("nlminb", "optim")}; for \code{"Liang"} also
      \code{sigma_floor = 1e-8}, \code{estimate_intercept = FALSE},
      \code{baseline_formula = ~1}. The imputation seed is
      \code{fit_method}'s \code{seed}. When calling \code{fit_method()}
      directly, give the number of imputations as \code{controls = list(m = 5)}
      or name the first argument (\code{method = "mi_lmm"}); otherwise R
      matches \code{m} to \code{method}.}
    \item{\code{Linear_increment_IP}}{\code{li_covariance =
      c("randomwalk", "rate_independence")},
      \code{inference = c("conditional", "patient_bootstrap")},
      \code{bootstrap_reps = 200L}, \code{keep_completed = FALSE},
      \code{leading_imputation = c("reverse_li", "error")}, and
      \code{unobserved_subjects = c("exclude", "error")}; for
      \code{"LMM"} also \code{lmm_method} and \code{optimizer}; for
      \code{"Liang"} also \code{sigma_floor}, \code{estimate_intercept}, and
      \code{baseline_formula}. The bootstrap seed is
      \code{fit_method}'s \code{seed}.}
    \item{\code{EHRJoint}}{\code{obs_link = c("logit", "probit")},
      \code{sigma_floor = 1e-6}, \code{retain_influence = FALSE},
      \code{rank_tol = 1e-9}.}
  }
  \code{Summary_stat} and \code{LMM} have no further controls; \code{LMM}
  uses \code{outcome_random_structure} and \code{outcome_random_intercept}.

  Both marker and visit versions of \code{Lin_Ying} and \code{Liang}
  use \code{inference = "stacked"}; this is the only supported inference
  setting for these four methods. The point estimators are unchanged by
  this covariance calculation. Lin-Ying's patient-level sandwich propagates
  the fitted rate regression and both empirical profile means, including
  the nearest-history outcome smoother. Validity still requires the
  nearest-history mean assumptions and adequate within-subject support.
  Liang's patient-level sandwich
  propagates the rate regression, Breslow baseline, frailty moments,
  centering weights and outcome equation. With \code{estimate_intercept = TRUE},
  Liang inference conditions on the specified outcome baseline basis.
  At an inactive frailty component, inference conditions on the selected
  no-frailty branch; it does not include branch-selection uncertainty, and
  regular Wald inference for the boundary variance is unavailable.

  The primary outcome covariance is returned in \code{vcov};
  \code{vcov(fit, all = TRUE)} also gives the available joint parameter
  covariance. \code{return_subject_scores = TRUE} for Lin-Ying and
  \code{retain_influence = TRUE} for Liang retain additional patient-level
  quantities for inspection. These inference settings apply to the direct
  Lin-Ying and Liang methods. Multiple-imputation and linear-increment
  methods retain their separately documented inference rules.

  For all three \code{Linear_increment_IP()} methods, the default
  \code{leading_imputation = "reverse_li"} reconstructs outcomes before the
  first observed outcome using a separately fitted reverse increment-rate
  regression. Only original observed adjacent pairs train that model; its
  predictors include the next outcome and requested visit/outcome covariates.
  LI-LMM uses original covariates and random-structure modifiers, avoiding
  generated random-effect basis columns; LI-IIRR uses the union of visit,
  outcome and random-link covariates. LI-Liang additionally includes its
  visit-count frailty terms. It recurses backward only over the leading
  missing run. The subsequent forward reconstruction is unchanged. Use
  \code{leading_imputation = "error"} to reject missing starting outcomes.
  No reverse model is fitted if leading reconstruction is unnecessary.

  The reverse procedure is a derived extension requiring a correctly specified
  reverse conditional-mean model and representative observed pairs conditional
  on the visit process. These assumptions do not follow from forward LI alone.
  Coefficients are pooled across time and visit gaps; no additional time basis
  is fitted for either increment model.

  The default \code{unobserved_subjects = "exclude"} excludes a whole person
  with one or more actual visits (finite time) and no originally observed
  outcome before either LI model or the downstream analysis is fitted. An observed
  outcome requires original \code{R = 1} and finite \code{Y}.
  \code{unobserved_subjects = "error"} retains the rejection policy.
  Public input validation still applies before this eligibility rule.
  Zero-visit placeholders and people with one observed visit remain.
  Multiple-visit subjects with at least one observed outcome remain eligible
  for forward/reverse reconstruction.

  Every actual visit of a retained person must have a finite outcome before
  fitting LMM, Liang or IIRR; this includes both Liang baseline modes. Insufficient or unidentified increment
  regressions, unresolved outcomes and invalid covariate designs cause errors.
  Original \code{R} values and all visits of retained people are preserved;
  all applicable visit, frailty, centering and outcome equations use the same cohort.
  Exclusion changes the analysis population and may change the visit/frailty
  distribution. LI and downstream model assumptions must hold in the retained cohort;
  this policy does not establish unbiasedness for the original input cohort.
  Diagnostics record \code{unobserved_subjects}, \code{n_subjects_input},
  \code{n_visits_input}, \code{n_subjects_excluded_unobserved},
  \code{n_visits_excluded_unobserved}, \code{analysis_population}, and
  \code{bootstrap_population}, as well as forward/backward imputation counts.
  Exclusions are reported as counts without subject identifiers.

  All three LI methods default to \code{inference = "conditional"}, which
  conditions on LI reconstruction; LI-Liang also conditions on its Stage 1
  estimates. Explicitly choosing \code{inference = "patient_bootstrap"}
  refits both required increment regressions and the downstream analysis in
  subject bootstrap samples drawn from the retained cohort, conditional on
  eligibility. LI-Liang additionally re-estimates its visit-count frailty terms.
  Bootstrap includes their estimation uncertainty, but does not correct
  selection bias or validate the model or missing-data assumptions. The same
  finite-outcome requirement applies to each refit.

  Delayed entry (\code{entry_col} with a nonzero entry time) is supported
  by \code{lin_ying_marker}, \code{lin_ying_visit}, \code{liang_marker},
  \code{liang_visit}, \code{jmvl_g_marker}, \code{jmvl_g_visit},
  \code{iirr_weighting_marker}, \code{iirr_weighting_visit},
  \code{iirr_balancing_marker}, \code{iirr_balancing_visit} and
  \code{ehrjoint}; the other wrapped methods stop with an error.
  \code{mi_liang} and \code{li_liang} support subject-specific follow-up
  through the ordinary Liang visiting-process estimator and individual risk sets.
  \code{mi_iirr} and \code{li_iirr} use a common
  follow-up window: when \code{censor_col} is supplied, the censoring time
  must be the same for every subject. \code{mi_lmm}, \code{mi_liang} and
  \code{mi_iirr} require the suggested packages \pkg{mice} and \pkg{pan}.
}
\value{
  An object of class \code{"cimehr_fit"} as returned by
  \code{\link{fit_method}}: a list with, among others, \code{method_key},
  \code{method}, \code{coefficients} (named numeric vector), \code{se},
  \code{vcov}, \code{diagnostics} (including
  \code{n_visits_at_entry_excluded}, \code{elapsed_seconds} and
  \code{warnings}), \code{variance_type}, \code{variance_label},
  \code{assumptions}, \code{parameter_table} and \code{call}. Methods with
  a visiting or observation model also return nuisance estimates, which
  \code{\link{summary_visiting}} and \code{\link{summary_observation}}
  tabulate, and an all-stage covariance where available
  (\code{vcov(fit, all = TRUE)}).
}
\seealso{
  \code{\link{fit_method}}, \code{\link{available_comparison_methods}},
  \code{\link{method_comparisons}}, \code{\link{summary_outcome}},
  \code{\link{coef.cimehr_fit}}.
}
\examples{
## Simulated EHR-type data: one row per visit, 150 subjects
set.seed(1)
one_subject <- function(i) {
  Z <- rbinom(1, 1, 0.5); X <- rnorm(1); C <- runif(1, 5, 10); b <- rnorm(1)
  k <- rpois(1, C * exp(-1 + 0.3 * Z + 0.5 * b))
  if (k == 0)
    return(data.frame(id = i, time = NA, Y = NA, R = NA, Z = Z, X = X, C = C))
  time <- sort(runif(k, 0, C))
  R <- rbinom(k, 1, plogis(0.5 + 0.3 * X))
  Y <- 1 + 0.5 * Z - 0.3 * X + 0.1 * time + b + rnorm(k, sd = 0.5)
  data.frame(id = i, time = time, Y = ifelse(R == 1, Y, NA), R = R,
             Z = Z, X = X, C = C)
}
dat <- do.call(rbind, lapply(1:150, one_subject))
vc <- c("Z", "X")

Summary_stat(dat, "mean", visit_covs = vc, long_covs = vc)

fit_lmm <- Linear_mixed_model(dat, visit_covs = vc, long_covs = vc,
                              outcome_random_covs = "Z")
coef(fit_lmm)

fit_liang <- Liang(dat, process = "visit", visit_covs = vc, long_covs = vc,
                   outcome_random_covs = "Z", censor_col = "C")
fit_liang$method_key
summary_visiting(fit_liang)

fit_lv <- fit_method("liang_visit", dat, visit_covs = vc, long_covs = vc,
                     outcome_random_covs = "Z", censor_col = "C")
all.equal(coef(fit_liang), coef(fit_lv))

coef(Lin_Ying(dat, visit_covs = vc, long_covs = vc, censor_col = "C"))
coef(JMVL_G(dat, visit_covs = vc, long_covs = vc, censor_col = "C"))
coef(Pairwise_likelihood(dat, visit_covs = vc, long_covs = vc))
coef(Inverse_intensity_rate_ratio_weighting(dat, visit_covs = vc, long_covs = vc,
                                            censor_col = "C", include_time = TRUE))
coef(IIRR_balancing(dat, process = "visit", visit_covs = vc, long_covs = vc,
                    censor_col = "C"))
coef(EHRJoint(dat, visit_covs = vc, long_covs = vc, outcome_random_covs = "Z",
              censor_col = "C", controls = list(obs_link = "probit")))
coef(Linear_increment_IP(dat, "LMM", visit_covs = vc, long_covs = vc,
                         outcome_random_covs = "Z"))

## LI-IIRR needs a common censoring time
dat_common <- dat
dat_common$C <- 10
coef(Linear_increment_IP(dat_common, "IIRR", visit_covs = vc, long_covs = vc,
                         censor_col = "C"))

## Multiple imputation requires the suggested packages mice and pan;
## m = 2 imputations only keeps the example fast
if (requireNamespace("mice", quietly = TRUE) &&
    requireNamespace("pan", quietly = TRUE)) {
  fit_mi <- Multiple_imputation_IP(dat, "LMM", visit_covs = vc, long_covs = vc,
                                   outcome_random_covs = "Z", m = 2)
  summary_outcome(fit_mi)
}

## The long names are aliases of the short ones
identical(Linear_mixed_model, LMM)
identical(Joint_modeling_visiting_and_longitudinal_Liang, Liang)
identical(Inverse_intensity_rate_ratio_weighting, IIRR)
identical(Inverse_intensity_rate_ratio_balancing, IIRR_balancing)
}
\keyword{models}
\keyword{regression}
