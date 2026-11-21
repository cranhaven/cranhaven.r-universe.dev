\name{stage_summaries}
\alias{stage_summaries}
\alias{summary_outcome}
\alias{summary_visiting}
\alias{summary_observation}
\alias{coef_stage}
\alias{extract_coefficient}
\title{Stage-Specific Parameter Tables}
\description{
  Tabulate the estimates of a fitted model by stage: the longitudinal
  outcome model, the visiting (event) process and the observation
  (recording) process. \code{coef_stage} returns the estimates of one
  stage as a named vector and \code{extract_coefficient} returns the table
  row of a single parameter.
}
\usage{
summary_outcome(object, ...)
summary_visiting(object, numerical_wald = FALSE, ...)
summary_observation(object, numerical_wald = FALSE, ...)

coef_stage(object, stage = c("outcome", "visiting", "observation"))

extract_coefficient(object, term,
    stage = c("outcome", "visiting", "observation"))
}
\arguments{
  \item{object}{A fitted object of class \code{"cimehr_fit"}, as returned
    by \code{\link{fit_method}}, the \link[=comparison_methods]{method
    wrappers} or \code{\link{CIMEHR}} and its variants.}
  \item{\dots}{Not used.}
  \item{numerical_wald}{Logical; default \code{FALSE}. For visiting and
    observation summaries, explicitly request separate arithmetic Wald
    limits from finite SEs of valid point estimates. This is a diagnostic
    calculation, not a calibrated boundary confidence interval. It does
    not change default intervals or validity flags.}
  \item{stage}{Stage to report: \code{"outcome"} (default),
    \code{"visiting"} or \code{"observation"}.}
  \item{term}{Character string naming one parameter, as in the
    \code{parameter} column of the stage table (for example \code{"Z"},
    \code{"gamma.Z"} or \code{"alpha.X"}). If it is not found,
    \code{paste0("beta.", term)} is tried, so \code{"Z"} selects
    \code{"beta.Z"} in a CIMEHR fit.}
}
\details{
  \code{summary_outcome(object)}, \code{summary_visiting(object)} and
  \code{summary_observation(object)} are the tables for the stages
  \code{"outcome"}, \code{"visiting"} and \code{"observation"}.
  The stages contain:
  \describe{
    \item{outcome}{The outcome-model coefficients in
      \code{object$coefficients}; for CIMEHR and CIMEHR-HL (\code{cimehr}
      and \code{cimehr_hl}) only the \code{beta.*} and \code{theta.*}
      entries. This is the table printed by \code{print} and returned by
      \code{summary} for a \code{"cimehr_fit"} object.}
    \item{visiting}{For \code{cimehr} and \code{cimehr_hl}, the
      \code{gamma.*} coefficients and the frailty variance
      \code{sigma2_zeta}; for the spline methods (\code{cimehr_spline} and
      \code{cimehr_spline_hl}), the same parameters taken from the spline
      fit. For the other methods, the visiting-process coefficients reported
      by the method (\code{gamma.*}) and, when reported, the frailty
      variance (\code{frailty_variance}) and a scalar baseline
      \code{Lambda}.}
    \item{observation}{For the CIMEHR methods (\code{cimehr},
      \code{cimehr_hl}, \code{cimehr_spline}, \code{cimehr_spline_hl}), the
      recording-model coefficients \code{alpha.*}, the random-link
      coefficients \code{delta.*} and the variance components
      \code{sigma2_q.*}. For the other methods, the recording-model
      coefficients \code{alpha.*} when the method fits one (for example
      \code{ehrjoint}).}
  }
  A stage that the method does not estimate gives a data frame with no
  rows (and \code{coef_stage} gives \code{numeric(0)}).

  Intervals in the outcome table are 95\% intervals based on
  \code{qt(0.975, df)} (normal quantiles when \code{df = Inf}); intervals
  in the visiting and observation tables are 95\% normal (Wald)
  intervals. Use \code{\link[=confint.cimehr_fit]{confint}} for another
  level of the outcome parameters. By default, intervals use every checked
  available covariance, including conditional covariance at a variance
  boundary. \code{confint(object, type = "regular")} restricts output to
  regular inference. Unavailable or unidentified parameter covariances do
  not acquire intervals; a boundary marker describes the fitted model and
  does not alone suppress an available outcome interval. If the fitted object
  explicitly records a covariance failure, existing finite numerical limits
  are retained with \code{inference_valid = FALSE} and
  \code{inference_status = "covariance_failure"}, rather than treated as
  validated inference.

  The selected-face CIMEHR sandwich does not estimate the sampling
  uncertainty of variance coordinates held fixed at zero. Their missing
  SEs are not replaced by zero and do not indicate failed point estimation.
  A finite supplied boundary SE is retained even when it does not support
  a calibrated interval. Numerical limits from such an SE are available
  only in separate columns when \code{numerical_wald = TRUE}; a warning
  identifies requested limits lacking a supported reference distribution.
  A missing SE is never imputed. Reporting fallbacks require finite,
  nonnegative covariance diagonals rather than replacing negative values
  by zero. No calibrated boundary-variance test or interval is implemented
  by this reporting option.
}
\value{
  \code{summary_outcome}, \code{summary_visiting} and
  \code{summary_observation} return a data frame with one row per
  parameter and the columns
  \item{parameter}{Parameter name.}
  \item{stage}{\code{"outcome"}, \code{"visiting"} or \code{"observation"}.}
  \item{estimate, se}{Estimate and standard error (\code{NA} when not
    available).}
  \item{df}{Degrees of freedom used for the interval (\code{Inf} for a
    normal reference).}
  \item{lower, upper}{Available numerical 95\% interval limits, with
    validity reported separately; \code{NA} when \code{ci_available} is
    \code{FALSE}.}
  \item{point_valid}{\code{TRUE} when the estimate is finite and the fit
    diagnostics report success, convergence and identification.}
  \item{inference_valid}{Whether inference is supported within its stated
    scope, separately from numerical availability. At zero frailty,
    \code{conditional_supported} gives \code{TRUE} for eligible computed
    conditional covariance under the fitted-model and selected-branch/face
    assumptions. Descriptive loading, relevance and collinearity screens do
    not control this flag, and missing descriptive diagnostics alone do not
    assign \code{NA}. Existing fit, estimability and covariance checks apply.
    \code{FALSE} denotes unavailable or invalid inference under those
    checks. Conditional support does not prove population identification,
    nominal coverage, or selection-adjusted coverage.}
  \item{inference_status}{For example \code{regular}, \code{conditional},
    \code{conditional_supported}, \code{not_assessed}, \code{unavailable},
    \code{covariance_failure}, or \code{fit_invalid}, according to covariance eligibility and fit
    checks. A descriptive warning does not replace
    \code{conditional_supported} for otherwise eligible zero-frailty
    inference.}
  \item{identification_status, identification_warning}{Diagnostic status
    and explanation of identification concerns. These describe design and
    fitted-information checks, not a proof of identification at the true
    parameter or a guarantee of coverage.}
  \item{ci_available}{\code{TRUE} when limits from the stated regular or
    conditional calculation are present, including retained numerical limits
    whose validity is \code{FALSE} or \code{NA}. This flag reports availability, not nominal
    coverage. Uncalibrated numerical boundary-variance limits requested
    separately do not set this flag. A missing covariance or SE is not
    replaced by zero or otherwise imputed.}
  \item{regular_inference_valid}{\code{TRUE} specifically when regular
    inference is available for that parameter and stage. Fixed-effect
    inference in an LMM is checked separately from the covariance-parameter
    Hessian. Visiting-regression inference is checked separately from
    downstream observation or outcome variance boundaries.}
  \item{regular_lower, regular_upper}{95\% regular interval limits;
    \code{NA} when \code{regular_inference_valid} is \code{FALSE}.}
  \item{se_available}{\code{TRUE} for a finite, nonnegative reported SE,
    including an SE whose validity is limited to a stated conditional
    scope. This flag alone does not assert nominal interval coverage.}
  \item{se_status}{Availability and, when known from the fitted object,
    the reason for a missing or invalid SE: \code{available}, \code{disabled},
    \code{not_computed}, \code{boundary_not_computed},
    \code{numerical_failure}, \code{identifiability_failure}, or
    \code{unknown}. A boundary flag alone does not establish a numerical
    failure. \code{unknown} is used when saved information cannot identify
    the cause.}
  \item{ci_status}{\code{regular}, \code{conditional},
    \code{numerical_only}, \code{covariance_failure}, \code{boundary_uncalibrated}, \code{se_unavailable},
    \code{fit_invalid}, \code{reference_unavailable}, or
    \code{inference_unavailable}, according to the point estimate, SE,
    reference distribution, and stated calculation scope. A
    \code{conditional} CI status describes the covariance calculation;
    \code{conditional_supported} inference status describes eligibility
    under the stated assumptions. After a recorded covariance failure,
    retained finite numerical limits have \code{numerical_only} status;
    this does not certify their covariance calculation. Descriptive concerns
    are recorded separately in \code{identification_status} and
    \code{identification_warning}.}
  \item{conditional_inference_available}{\code{TRUE} when a valid point
    estimate and finite SE support an interval in the reported conditional
    scope, or regular inference is available.}
  \item{conditional_lower, conditional_upper}{95\% limits using that
    covariance; \code{NA} when it is unavailable. At a selected variance
    boundary they hold the chosen branch or face fixed, omit selection
    uncertainty and are not selection-adjusted intervals.}
  \item{inference_scope, inference_reason}{Description of the covariance
    and the availability or limitation of regular inference. For a CIMEHR
    fit at a selected zero-frailty branch, stacked inference includes the
    remaining identifiable nuisance quantities and is conditional on that
    branch and variance face (see \code{\link{CIMEHR}}).}
  \item{parameter_boundary}{\code{TRUE} for a reported variance parameter
    on its zero boundary. It does not imply that other parameters or the
    entire fit failed.}
  \item{fit_boundary}{\code{TRUE} when the fitted model reports a variance
    or frailty boundary. This is separate from convergence and SE availability.}
  \item{boundary}{Legacy flag retained for compatibility. In ordinary
    stage and outcome tables this describes the fit; in spline nuisance
    tables it describes the parameter. Use \code{parameter_boundary} and
    \code{fit_boundary} for consistent meanings across methods.}
  \item{variance_type}{Type of variance estimate, for example
    \code{"hc3"}, \code{"model_based"}, \code{"plug_in_conditional"},
    \code{"stacked_sandwich"} or \code{"stacked_branch_conditional"}.}
  The visiting and observation tables of the spline methods
  (\code{cimehr_spline}, \code{cimehr_spline_hl}) add the columns
  \code{scale}, \code{variance_label}, \code{conditional_inference_valid},
  and \code{se_source}. The legacy spline column
  \code{conditional_inference_valid} is an alias for conditional interval
  availability, not a claim of selection-adjusted coverage. Tables for \code{cimehr_hl} and
  \code{cimehr_spline_hl} add \code{method_key}, \code{variant} and
  \code{native_base_method}.

  With \code{numerical_wald = TRUE}, visiting and observation tables also
  contain \code{numerical_lower}, \code{numerical_upper},
  \code{numerical_wald_available}, and \code{numerical_wald_scope}.
  These are separately labelled arithmetic limits; they never alter
  \code{lower}, \code{upper}, or inference-validity flags, and do not
  establish boundary calibration.

  \code{coef_stage} returns the \code{estimate} column as a numeric vector
  named by \code{parameter}.

  \code{extract_coefficient} returns the one-row data frame of the
  requested parameter and stops with an error if it is not in the stage.
}
\seealso{
  \code{\link{fit_method}}, \code{\link{coef.cimehr_fit}},
  \code{\link{comparison_methods}}.
}
\examples{
data("cimehr_example", package = "CIMEHR")
dat <- merge(cimehr_example$participants, cimehr_example$visits,
             by = "id", all.x = TRUE)
dat <- dat[dat$id <= 200, ]  # 200 of the 1,000 participants keeps the example fast
dat <- dat[order(dat$id, dat$time, na.last = TRUE), ]
rownames(dat) <- NULL

fit <- fit_method("cimehr", dat, visit_covs = c("Z", "X"),
                  obs_fixed_covs = c("Z", "X"), obs_random_covs = "Z",
                  long_covs = c("Z", "X"), outcome_random_covs = "Z",
                  censor_col = "C")
summary_outcome(fit)
summary_visiting(fit)
summary_observation(fit)
coef_stage(fit, "visiting")
extract_coefficient(fit, "Z")
extract_coefficient(fit, "alpha.X", stage = "observation")

# A model without an observation stage returns an empty table.
fit_lmm <- LMM(dat, long_covs = c("Z", "X"), censor_col = "C")
nrow(summary_observation(fit_lmm))
}
\keyword{models}
