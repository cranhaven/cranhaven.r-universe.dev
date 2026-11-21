\name{fit_method}
\alias{fit_method}
\alias{method_comparisons}
\alias{available_comparison_methods}
\alias{cimehr_methods}
\title{Fit CIMEHR or a Comparison Method Through the Common Interface}
\description{
  \code{fit_method()} fits one method, chosen by method key or label, to
  visit-level electronic health record data with a common set of column and
  covariate arguments. \code{method_comparisons()} fits several methods to the
  same data. \code{available_comparison_methods()} returns the method registry
  and \code{cimehr_methods()} returns a default or complete set of method
  keys.
}
\usage{
fit_method(method, data, id = "id", time = "time", outcome = "Y",
           obs_indicator = "R", visit_covs = NULL, long_covs = NULL,
           outcome_random_covs = NULL, obs_random_covs = outcome_random_covs,
           censor_col = NULL, entry_col = NULL, obs_fixed_covs = visit_covs,
           cimehr_visit_covs = visit_covs, outcome_fixed_structure = NULL,
           outcome_random_structure = NULL, outcome_random_intercept = TRUE,
           focal = if (length(long_covs)) long_covs[1L] else NULL,
           seed = 1L, controls = list(), ...)

method_comparisons(data, methods = cimehr_methods("selected"),
                   controls = list(), ...)

available_comparison_methods()

cimehr_methods(mode = c("selected", "all"))
}
\arguments{
  \item{method}{a single method: a method key from the \code{method} column
    of \code{available_comparison_methods()} (for example \code{"lmm"},
    \code{"liang_visit"}, \code{"cimehr"}), a label from the \code{label}
    column (for example \code{"LMM"}, \code{"Liang (visit)"},
    \code{"CIMEHR"}) or one of the aliases \code{"IIRR-M"}
    (\code{iirr_weighting_marker}), \code{"IIRR-V"}
    (\code{iirr_weighting_visit}), \code{"Liang-M"} (\code{liang_marker}),
    \code{"Liang-V"} (\code{liang_visit}), \code{"LI+Liang"}
    (\code{li_liang}), \code{"LI+IIRR"} (\code{li_iirr}),
    \code{"CIMEHR (updated)"} (\code{cimehr}), \code{"CIMEHR_spline"}
    (\code{cimehr_spline}), \code{"CIMEHR_HL"} (\code{cimehr_hl}) and
    \code{"CIMEHR_spline_HL"} (\code{cimehr_spline_hl}). Numeric input is an
    error.}
  \item{data}{a data frame with one row per visit; see \sQuote{Data format}.}
  \item{id, time, outcome, obs_indicator}{names of the subject identifier,
    visit time, outcome and recording indicator columns.}
  \item{visit_covs}{character vector of subject-level covariates for the
    visiting (or recorded-marker) event process. It is also the default for
    \code{obs_fixed_covs} and \code{cimehr_visit_covs}.}
  \item{long_covs}{character vector of covariates with fixed effects in the
    outcome model. Their coefficients are the main results.}
  \item{outcome_random_covs}{character vector of covariates that carry outcome
    random effects in addition to the random intercept, for methods that model
    outcome random effects. \code{NULL} means none.}
  \item{obs_random_covs}{character vector of covariates that link the random
    effects in the recording (observation) model of the CIMEHR methods
    (\code{cimehr}, \code{cimehr_hl}, \code{cimehr_spline},
    \code{cimehr_spline_hl}).}
  \item{censor_col}{name of the censoring-time column (constant within
    subject). Required by the CIMEHR methods (\code{cimehr},
    \code{cimehr_hl}, \code{cimehr_spline}, \code{cimehr_spline_hl}). When
    \code{NULL}, methods with a censoring argument use the largest observed
    visit time as a common censoring time.}
  \item{entry_col}{optional name of the entry-time column (constant within
    subject). When \code{NULL} there is no left truncation. Only
    \code{lin_ying_marker}, \code{lin_ying_visit}, \code{liang_marker},
    \code{liang_visit}, \code{jmvl_g_marker}, \code{jmvl_g_visit},
    \code{iirr_weighting_marker}, \code{iirr_weighting_visit},
    \code{iirr_balancing_marker}, \code{iirr_balancing_visit},
    \code{ehrjoint} and the CIMEHR methods accept delayed entry; the others
    stop if any entry time differs from 0.}
  \item{obs_fixed_covs}{character vector of fixed covariates in the recording
    model of the CIMEHR methods and \code{ehrjoint}. For EHRJoint these must
    be subject-level, time-invariant numeric covariates. The default uses
    \code{visit_covs}, for EHRJoint after any \code{controls} override;
    \code{character()} specifies an intercept-only
    recording model. EHRJoint's stacked covariance uses this recording design
    separately from the visiting design.}
  \item{cimehr_visit_covs}{character vector of visiting-process covariates used
    by the CIMEHR methods.}
  \item{outcome_fixed_structure}{optional list declaring fixed outcome
    covariates as affine functions of time (CIMEHR methods); see
    \code{\link{CIMEHR}}.}
  \item{outcome_random_structure}{optional list describing the outcome
    random-effect design (\code{lmm}, \code{oa_lmm}, \code{va_lmm},
    \code{mi_lmm}, \code{li_lmm} and the CIMEHR methods).}
  \item{outcome_random_intercept}{logical; include an outcome random intercept
    (\code{lmm}, \code{oa_lmm}, \code{va_lmm}, \code{mi_lmm}, \code{li_lmm}
    and the CIMEHR methods).}
  \item{focal}{name of the focal outcome coefficient. The summary-statistic
    and mixed-model methods (\code{summary_min}, \code{summary_mean},
    \code{summary_median}, \code{summary_max}, \code{lmm}, \code{oa_lmm},
    \code{va_lmm}) set \code{diagnostics$ok = FALSE} when it is missing or
    not finite.}
  \item{seed}{integer seed for methods that use random numbers: subject-pair
    sampling in \code{pairlik}, imputation in \code{mi_lmm}, \code{mi_liang}
    and \code{mi_iirr} and the optional bootstrap in \code{li_lmm},
    \code{li_liang} and \code{li_iirr}. The global random number state is
    restored afterwards.}
  \item{controls}{for \code{fit_method()}, a named list of method-specific
    arguments passed to the method (for example
    \code{list(obs_link = "probit")} for \code{ehrjoint} or
    \code{list(inference = "none")} for \code{cimehr}). Names must be unique,
    must be arguments of the chosen method, and may not repeat the data or
    column arguments above. For \code{method_comparisons()}, a list of such
    lists named by the value supplied in \code{methods} or by the method key
    (for example \code{"cimehr"} or \code{"liang_marker"}); a name equal to
    the supplied value takes precedence.}
  \item{\dots}{for \code{fit_method()}, further method-specific arguments,
    merged into \code{controls}. For \code{method_comparisons()}, arguments
    passed to \code{fit_method()} for every method.}
  \item{methods}{character vector of method keys, labels or aliases, as for
    \code{method}. Numbers and unknown names are an error for the whole
    call.}
  \item{mode}{\code{"selected"} for the default comparison set, \code{"all"}
    for every registered method.}
}
\details{
  \code{fit_method()} checks the data, removes visits that coincide with the
  entry time, resolves the method, passes the common arguments that the method
  accepts together with \code{controls}, and returns a standardized result.
  Warnings raised while fitting are collected in
  \code{diagnostics$warnings} and are not printed, except that a fit with
  zero-frailty identification warnings signals one warning of class
  \code{cimehr_identification_warning}. Methods are identified only by key, label or
  alias: a numeric \code{method} is an error, and so is a string that is
  neither a key, a label nor an alias.

  Fits from the CIMEHR variants, Liang-V, Liang-M, MI+Liang, LI+Liang,
  and EHRJoint retain input data and evaluated fitting arguments in
  \code{refit} for the optional participant bootstrap in
  \code{\link{meanfit}}. Request it with
  \code{meanfit(fit, bootstrap = TRUE, B = 50L)}; it is disabled by
  default and separate from method-specific coefficient inference.
  The retained data include any zero-visit participant rows supplied to
  the fit. Older saved fits without this component must be refitted
  before requesting bootstrap mean curves.

  Several estimators come in a marker version, whose event process is the
  recorded outcomes (visits with \code{R = 1}), and a visit version, whose
  event process is all visits: \code{lin_ying_marker}/\code{lin_ying_visit},
  \code{liang_marker}/\code{liang_visit},
  \code{jmvl_g_marker}/\code{jmvl_g_visit},
  \code{iirr_weighting_marker}/\code{iirr_weighting_visit},
  \code{iirr_balancing_marker}/\code{iirr_balancing_visit}. The key fixes the
  process; setting \code{process} in \code{controls} to the other process is
  an error. \code{mi_iirr} and \code{li_iirr}
  require common follow-up and stop when the censoring time varies between
  subjects. \code{mi_lmm}, \code{mi_liang} and \code{mi_iirr} need the
  suggested packages \pkg{mice} and \pkg{pan}.

  \code{li_lmm}, \code{li_liang} and \code{li_iirr} default to
  \code{leading_imputation = "reverse_li"}:
  a separately fitted reverse increment-rate model reconstructs leading
  missing outcomes from the subject's first observed outcome. It uses only
  original observed adjacent pairs for fitting, with the next outcome
  and requested visit/outcome covariates as predictors. LI-LMM uses original
  covariates and random-structure modifiers instead of generated random-effect
  basis columns; LI-IIRR uses the union of visit, outcome and random-link
  covariates. LI-Liang additionally includes its visit-count frailty terms.
  Forward reconstruction after the first observation is unchanged. Set
  \code{controls = list(leading_imputation = "error")} to reject missing
  starting outcomes. No reverse model is fitted unless needed.

  Reverse reconstruction requires additional reverse conditional-mean and
  missingness assumptions conditional on the visit process. It is not an
  algebraic inversion or an automatic consequence of forward LI. Both models
  pool increment-rate coefficients across time and gaps.

  The default \code{controls = list(unobserved_subjects = "exclude")} excludes
  a whole person with one or more actual visits (finite time) and no originally
  observed outcome before fitting LI or the downstream analysis. An observed outcome
  requires original \code{R = 1} and finite \code{Y}. Set
  \code{controls = list(unobserved_subjects = "error")} to retain the rejection
  policy. Public input validation still applies before this eligibility rule.
  Zero-visit placeholders and people with one observed visit are retained;
  multiple-visit subjects with an observed anchor remain eligible for LI.
  Every actual visit of every retained person must have a finite outcome
  before LMM, Liang or IIRR is fitted, including both Liang baseline modes. Insufficient or unidentified
  increment regressions, unresolved outcomes and invalid covariate designs
  cause errors. Visits and original \code{R} values of retained people are
  preserved across all stages. Selection changes the analysis population and
  can change the visit/frailty distribution. LI and downstream model assumptions must
  hold in the retained cohort; this policy does not establish unbiasedness for
  the original input cohort.

  Diagnostics report \code{unobserved_subjects}, \code{n_subjects_input},
  \code{n_visits_input}, \code{n_subjects_excluded_unobserved},
  \code{n_visits_excluded_unobserved}, \code{analysis_population}, and
  \code{bootstrap_population}. Exclusions are reported as counts without
  subject identifiers.

  The default covariance for all three LI methods conditions on LI
  reconstruction; LI-Liang also conditions on its Stage 1 estimates.
  Explicitly request \code{controls = list(inference = "patient_bootstrap")}
  to refit both required increment regressions and the downstream analysis in
  each subject bootstrap sample from the retained cohort, conditional on
  eligibility. LI-Liang additionally re-estimates its visit-count frailty
  terms. This includes their estimation uncertainty but does not
  correct selection bias or validate the missing-data assumptions. Every refit must meet the
  same finite-outcome requirement. Imputation diagnostics report forward and
  backward counts, the leading-imputation policy and whether a backward model
  was used.

  \code{method_comparisons()} calls \code{fit_method()} for each element of
  \code{methods}. An error while fitting one method does not stop the
  others.

  \code{cimehr_methods("selected")} returns \code{lmm}, \code{summary_median},
  \code{iirr_weighting_visit}, \code{liang_visit},
  \code{iirr_weighting_marker}, \code{liang_marker}, \code{li_iirr},
  \code{li_liang}, \code{ehrjoint}, \code{cimehr}, \code{cimehr_spline},
  \code{cimehr_hl} and \code{cimehr_spline_hl}.
}
\section{Data format}{
  One row per visit with columns \code{id}, \code{time}, \code{Y} (outcome,
  \code{NA} when not recorded), \code{R} (1 if the outcome was recorded at the
  visit, else 0), numeric subject-level covariates (constant within subject),
  \code{C} (censoring time) and optionally \code{E} (entry time). A subject
  without visits has exactly one row with \code{time}, \code{Y} and \code{R}
  equal to \code{NA}. Visits must lie in \code{[E, C]}. Covariates must be
  numeric; encode factors with \code{\link[stats]{model.matrix}}.

  The risk set of the visit process is \code{(E, C]}. A visit exactly at the
  entry time is not part of the visit process and is dropped before fitting
  (a subject left without visits becomes a zero-visit row); the number
  dropped is returned in \code{diagnostics$n_visits_at_entry_excluded}.
}
\value{
  \code{fit_method()} returns a list of class \code{"cimehr_fit"} (with
  \code{"cimehr_spline_fit"} added for \code{cimehr_spline} and
  \code{cimehr_spline_hl} and \code{"cimehr_hl_fit"} for \code{cimehr_hl} and
  \code{cimehr_spline_hl}). Its main components are
  \item{method_key, method, estimand}{the method key, name and a description
    of its target.}
  \item{coefficients}{named numeric vector of estimates. For \code{cimehr} and
    \code{cimehr_hl} it contains all stages: outcome (\code{beta.*},
    \code{theta.*}), visiting (\code{gamma.*}, \code{sigma2_zeta}) and
    recording (\code{alpha.*}, \code{delta.*}, \code{sigma2_q.*})
    parameters; \code{cimehr_spline} and \code{cimehr_spline_hl} also report
    spline coefficients (\code{baseline.*}).}
  \item{se, vcov}{standard errors and covariance matrix of the outcome
    parameters.}
  \item{covariance_factor}{for CIMEHR methods, a compact factor of the
  outcome participant influences, when uncertainty is computed. It supports
  stable contrast calculations without storing every participant influence.}
  \item{vcov_all}{joint covariance of all estimated stages, for methods that
    provide one (otherwise absent); also available as
    \code{vcov(fit, all = TRUE)}.}
  \item{variance_type, variance_label}{standardized description of the
    variance estimator.}
  \item{diagnostics}{list including \code{ok}, \code{warnings},
    \code{elapsed_seconds}, \code{package_version} and
    \code{n_visits_at_entry_excluded}, plus
    method-specific entries.}
  \item{parameter_table}{outcome-stage table as returned by
    \code{\link{summary_outcome}}.}
  \item{assumptions, call}{working assumptions of the method and the matched
    call.}
  Further method-specific components (for example \code{fit},
  \code{nuisance}, \code{inference}) may be present.

  \code{method_comparisons()} returns a list of such fits named by method
  label. A method that fails is returned as a list of class
  \code{"cimehr_failure"} with components \code{method} and \code{error}.

  \code{available_comparison_methods()} returns a data frame with one row per
  method; columns include \code{method} (the method key), \code{label},
  \code{native_estimand}, \code{requires_common_followup},
  \code{sampling_event_source}, \code{se_scope}, \code{variance_label},
  \code{base_method} and \code{native_base_method} (method keys; for
  example \code{"cimehr"} for \code{cimehr_hl}).

  \code{cimehr_methods()} returns a character vector of method keys.
}
\seealso{
  \code{\link{summary_outcome}}, \code{\link{summary_visiting}},
  \code{\link{summary_observation}}, \code{\link{extract_coefficient}};
  \code{\link{CIMEHR}}, \code{\link{CIMEHR_spline}} and the method wrappers
  such as \code{\link{LMM}} and \code{\link{Liang}}.
}
\examples{
data("cimehr_example", package = "CIMEHR")
dat <- merge(cimehr_example$participants, cimehr_example$visits,
             by = "id", all.x = TRUE)
dat <- dat[dat$id <= 200, ]  # 200 of the 1,000 participants keeps the example fast
dat <- dat[order(dat$id, dat$time, na.last = TRUE), ]
rownames(dat) <- NULL

available_comparison_methods()[, c("method", "label")]

fit <- fit_method("cimehr", dat, visit_covs = c("Z", "X"),
                  obs_fixed_covs = c("Z", "X"), obs_random_covs = "Z",
                  long_covs = c("Z", "X"), outcome_random_covs = "Z",
                  censor_col = "C")
summary_outcome(fit)
dim(vcov(fit, all = TRUE))

fit_lmm <- fit_method("lmm", dat, long_covs = c("Z", "X"),
                      outcome_random_covs = "Z", censor_col = "C")
coef(fit_lmm)
cimehr_methods()

## Fit several methods to the same data in one call
fits <- method_comparisons(dat, methods = c("lmm", "liang_visit"),
                           visit_covs = c("Z", "X"), long_covs = c("Z", "X"),
                           censor_col = "C")
lapply(fits, coef)
}
\keyword{models}
\keyword{regression}
