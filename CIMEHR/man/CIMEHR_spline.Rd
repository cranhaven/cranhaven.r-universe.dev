\name{CIMEHR_spline}
\alias{CIMEHR_spline}
\alias{CIMEHR_spline_HL}
\alias{predict.cimehr_spline_fit}
\title{CIMEHR with a Spline Baseline and Spline Trajectories}
\description{
  \code{CIMEHR_spline} (method key \code{cimehr_spline}) is the CIMEHR
  estimator with a regression-spline baseline mean and optional spline
  trajectory contrasts for selected covariates, so that the outcome mean
  over time can be estimated and predicted. Stages 1 and 2 are those of
  \code{\link{CIMEHR}}. \code{CIMEHR_spline_HL} (method key
  \code{cimehr_spline_hl}) uses the higher-order corrected posterior
  moments of \code{\link{CIMEHR_HL}}. The \code{predict} method returns
  fitted mean trajectories.
}
\usage{
CIMEHR_spline(data, id_col = "id", time_col = "time", y_col = "Y", r_col = "R",
    censor_col = "C", covars_visit_XV = NULL, covars_outcome_fixed_XY = NULL,
    outcome_fixed_structure = NULL, covars_outcome_random_link_ZY = NULL,
    outcome_random_structure = NULL, outcome_random_intercept = TRUE,
    covars_obs_fixed_XO = NULL, covars_obs_random_link_ZO = NULL,
    trajectory_covars = NULL, spline_type = c("natural", "bspline"),
    baseline_df = 5L, trajectory_df = 3L, baseline_knots = NULL,
    trajectory_knots = NULL, boundary_knots = NULL, reference_time = NULL,
    spline_degree = 3L, stage3_chunk_size = 100000L,
    inference = c("stacked", "conditional", "none"), nuisance_fit = NULL,
    retain_nuisance_fit = FALSE, retain_influence = FALSE, entry_col = NULL, ...)

CIMEHR_spline_HL(data, id_col = "id", time_col = "time", y_col = "Y", r_col = "R",
    censor_col = "C", covars_visit_XV = NULL, covars_outcome_fixed_XY = NULL,
    outcome_fixed_structure = NULL, covars_outcome_random_link_ZY = NULL,
    outcome_random_structure = NULL, outcome_random_intercept = TRUE,
    covars_obs_fixed_XO = NULL, covars_obs_random_link_ZO = NULL,
    trajectory_covars = NULL, spline_type = c("natural", "bspline"),
    baseline_df = 5L, trajectory_df = 3L, baseline_knots = NULL,
    trajectory_knots = NULL, boundary_knots = NULL, reference_time = NULL,
    spline_degree = 3L, stage3_chunk_size = 100000L,
    inference = c("stacked", "conditional", "none"), nuisance_fit = NULL,
    retain_nuisance_fit = FALSE, retain_influence = FALSE, entry_col = NULL, ...)

\method{predict}{cimehr_spline_fit}(object, newdata = NULL, times = NULL,
    type = c("population", "observed"), kappa = NULL, se.fit = TRUE,
    level = 0.95, aggregate = FALSE, weights = NULL, bandwidth = NULL, ...)
}
\arguments{
  \item{data}{A data frame with one row per visit, in the format described
    in \code{\link{CIMEHR}}.}
  \item{id_col, time_col, y_col, r_col, censor_col}{Names of the subject
    identifier, visit time, outcome, recording indicator (1 if recorded, 0
    otherwise) and censoring-time columns.}
  \item{covars_visit_XV}{Character vector of subject-level covariates
    \eqn{X_V} of the visiting rate model (no intercept), or \code{NULL}.}
  \item{covars_outcome_fixed_XY}{Character vector of outcome covariates
    \eqn{X_Y} with fixed coefficients \eqn{\beta}, or \code{NULL}. Columns
    must be constant within subject unless declared in
    \code{outcome_fixed_structure}. Do not include a constant column; the
    baseline spline contains the intercept.}
  \item{outcome_fixed_structure}{\code{NULL} or a list declaring
    time-affine outcome covariates, in the format described in
    \code{\link{CIMEHR}}. Here \code{columns} must list every covariate of
    \code{covars_outcome_fixed_XY}, in the same order (use
    \code{data.frame(modifier = "X", power = 0)} for a constant covariate
    \code{X}), and \code{time_origin} and \code{time_scale} must be
    given.}
  \item{covars_outcome_random_link_ZY}{Character vector of subject-level
    columns \eqn{Z_Y} multiplied by \eqn{\kappa_i}, with coefficients
    \eqn{\theta}. Including the name of the time column adds a latent-linked
    time slope (\code{theta.time}) in the time scale
    \eqn{(t - t_{ref})/w}{(t - reference_time)/w}, where \eqn{w} is the
    width of \code{boundary_knots}.}
  \item{outcome_random_structure}{\code{NULL} or a list declaring the
    columns of \eqn{Z_Y}, in the format described in \code{\link{CIMEHR}};
    \code{time_origin} and \code{time_scale} must be given.}
  \item{outcome_random_intercept}{Logical. If \code{TRUE} (default),
    \eqn{Z_Y} contains a constant column (\code{theta.(Intercept)}).}
  \item{covars_obs_fixed_XO, covars_obs_random_link_ZO}{Character vectors of
    subject-level covariates \eqn{X_O} and \eqn{Z_O} of the recording model,
    each added to an intercept; see \code{\link{CIMEHR}}.}
  \item{trajectory_covars}{Subset of \code{covars_outcome_fixed_XY} whose
    association with the outcome may change over time. Each receives spline
    contrasts \eqn{S(t) - S(t_{ref})}{S(t) - S(reference_time)} multiplied by
    the covariate, so its \code{beta} coefficient is the association at
    \code{reference_time}. A covariate with a time-varying path in
    \code{outcome_fixed_structure} cannot be listed.}
  \item{spline_type}{\code{"natural"} (\code{\link[splines]{ns}}, default)
    or \code{"bspline"} (\code{\link[splines]{bs}}).}
  \item{baseline_df, trajectory_df}{Integer basis dimensions (at least 2) of
    the baseline and trajectory splines. Ignored when the corresponding
    knots are supplied.}
  \item{baseline_knots, trajectory_knots}{Optional internal knots, strictly
    inside \code{boundary_knots}.}
  \item{boundary_knots}{Two increasing boundary knots. Default: the range of
    0, the visit times and the censoring times.}
  \item{reference_time}{Time at which the trajectory contrasts are zero and
    the origin of the default latent-linked time slope; must lie within
    \code{boundary_knots}. Default: the lower boundary knot.}
  \item{spline_degree}{Polynomial degree of B-splines; used only when
    \code{spline_type = "bspline"} (natural splines are cubic).}
  \item{stage3_chunk_size}{Positive integer; number of recorded outcome rows
    processed at a time when the Stage-3 design is built (controls
    memory).}
  \item{inference}{\code{"stacked"} (default): all-stage stacked sandwich;
    \code{"conditional"}: Stage-3 patient sandwich that holds Stages 1 and 2
    fixed; \code{"none"}: no covariance.}
  \item{nuisance_fit}{Optional Stages 1-2 fit (class
    \code{"cimehr_laplace_nuisance"}) from an earlier fit with
    \code{retain_nuisance_fit = TRUE}, found in \code{fit$fit$nuisance_fit}.
    It is reused instead of refitting Stages 1 and 2, for example to change
    the spline settings. The data, the visiting and recording designs,
    \code{time_precision}, \code{pairwise_control} and the method variant
    (ordinary or HL) must match the original fit, otherwise an error is
    raised.}
  \item{retain_nuisance_fit}{Logical; keep the Stages 1-2 fit in
    \code{fit$fit$nuisance_fit}.}
  \item{retain_influence}{Logical; keep patient-level influence functions in
    \code{fit$fit$stacked_sandwich} and Stage-3 internals in
    \code{fit$fit$stage3}.}
  \item{entry_col}{Optional name of a subject-level entry-time column for
    delayed entry; \code{NULL} means no left truncation.}
  \item{\dots}{For \code{CIMEHR_spline} and \code{CIMEHR_spline_HL}: the
    named Stage 1-2 controls \code{time_precision} (default 0),
    \code{pairwise_control} (default \code{list()}) and
    \code{derivative_step} (default \code{2e-5}), with the meanings given
    in \code{\link{CIMEHR}}; any other name is an error. For
    \code{predict}: additional arguments are rejected for
    \code{type = "population"} and ignored for \code{type = "observed"}.}
  \item{object}{A fit from \code{CIMEHR_spline} or \code{CIMEHR_spline_HL}.}
  \item{newdata}{Data frame with one row per covariate profile. It must
    contain the fixed covariates (or their modifiers when
    \code{outcome_fixed_structure} is used), the trajectory covariates and,
    for \code{type = "observed"}, the random-link covariates. For
    \code{type = "population"}, \code{NULL} uses one stored reference
    profile per retained subject; a function returning profiles at each
    requested time is also accepted, as in \code{\link{meanfit}}. For
    \code{type = "observed"}, an explicit data frame is required.}
  \item{times}{Finite numeric prediction times in the fitted time units.
    For \code{type = "population"}, times must lie within the stored
    follow-up domain, and \code{NULL} selects 101 equally spaced points
    in that domain. For \code{type = "observed"}, supply the times
    explicitly.}
  \item{type}{\code{"population"} (default) sets the latent term
    \eqn{\kappa_i Z_Y^\top\theta}{kappa_i Z_Y' theta} to zero;
    \code{"observed"} includes it using \code{kappa}.}
  \item{kappa}{For \code{type = "observed"}: one finite value of
    \eqn{\kappa} per row of \code{newdata}, for example taken from
    \code{fit$fit$outcome_functionals$kappa}, or from
    \code{fit$fit$outcome_functionals$compensation_multiplier} at a zero
    frailty variance.}
  \item{se.fit}{Logical; add standard errors and pointwise confidence
    limits. Requires a fit with a covariance
    (\code{inference} not \code{"none"}).}
  \item{level}{Confidence level of the pointwise limits.}
  \item{aggregate}{For \code{type = "population"}, average over profiles
    at each time. The default \code{FALSE} returns one curve per profile,
    including each stored subject profile when \code{newdata = NULL}.
    With \code{TRUE}, equal profile weights are used unless
    \code{weights} is supplied. Not used for \code{type = "observed"}.}
  \item{weights}{For population predictions with \code{aggregate = TRUE},
    one finite nonnegative weight per profile, with positive sum. Weights
    are normalized to sum to one. Not used for
    \code{type = "observed"}.}
  \item{bandwidth}{Accepted for the common population-trajectory interface;
    if supplied, must be positive and finite. The fitted spline basis is
    evaluated directly, so spline predictions do not use smoothing. Not
    used for \code{type = "observed"}.}
}
\details{
  Stages 1 and 2 (shared-frailty visiting model with Laplace posterior for
  the latent variable \eqn{U_i}; pairwise probit recording model giving
  \eqn{\omega_i} and \eqn{\kappa_i}) are described in \code{\link{CIMEHR}}.
  Stage 3 fits
  \deqn{E(Y_{ij}) = B(t_{ij})^\top b + X_{Y,i}(t_{ij})^\top\beta +
    \sum_g G_{ig}\{S(t_{ij}) - S(t_{ref})\}^\top\phi_g +
    \kappa_i Z_{Y,i}(t_{ij})^\top\theta}{E(Y_ij) = B(t_ij)' b +
    X_Yi(t_ij)' beta + sum_g G_ig (S(t_ij) - S(t_ref))' phi_g +
    kappa_i Z_Yi(t_ij)' theta}
  by least squares over all recorded outcomes, where \eqn{B} is the
  baseline basis (with intercept; coefficients \code{baseline.S0.k}),
  \eqn{G} are the \code{trajectory_covars} and \eqn{S} is the trajectory
  basis without intercept (coefficients \code{trajectory.<covariate>.Sc.k}).
  \subsection{Choosing the spline}{
    The baseline \eqn{B} and the trajectory basis \eqn{S} are chosen by
    the user. \code{spline_type} selects natural cubic splines
    (\code{\link[splines]{ns}}) or B-splines (\code{\link[splines]{bs}}
    of degree \code{spline_degree}); \code{baseline_df} and
    \code{trajectory_df} set the number of basis functions, with interior
    knots at quantiles of the observed visit times; \code{baseline_knots}
    and \code{trajectory_knots} place the interior knots explicitly (the
    corresponding \code{df} is then ignored); \code{boundary_knots} fixes
    the boundary knots, which also set the time scale \eqn{w} of random
    slopes in time. Through \code{\link{fit_method}} the same arguments
    are given in \code{controls}, for example
    \code{fit_method("cimehr_spline", dat, ..., controls = list(spline_type
    = "bspline", spline_degree = 2L, baseline_knots = c(10, 20, 30)))}.
  }

  The basis dimensions are fixed; unless \code{boundary_knots},
  \code{baseline_knots} and (with trajectory covariates)
  \code{trajectory_knots} are all supplied, inference is conditional on the
  knots selected from the observed visit times.

  With \code{inference = "stacked"} the sandwich propagates the estimation
  of Stages 1 and 2 through \eqn{\kappa_i}. At a variance boundary
  (estimated \eqn{\sigma^2_\zeta}{sigma2_zeta} or some
  \eqn{\sigma^2_q}{sigma2_q} equal to zero) the covariance is conditional
  on the selected face and \code{diagnostics$regular} is \code{FALSE}.
  Summary tables distinguish a finite reported SE, available intervals and
  regular inference. The default \code{confint} retains a checked
  available fixed-face interval and its scope; that interval omits
  face-selection uncertainty. \code{confint(fit, type = "regular")}
  selects regular intervals only. Stage-1 summaries use their own
  covariance checks rather than downstream variance-face regularity.
  At a zero estimate of \eqn{\sigma^2_\zeta}{sigma2_zeta}, the
  compensation coordinates and identification checks in
  \code{\link{CIMEHR}} apply (see its \sQuote{Zero visiting frailty}).
  The column-space, relevance, compensation-collinearity and loading
  diagnostics in \code{\link{CIMEHR}} also apply. Diagnostic warnings retain
  computable covariance, SEs and interval limits. Successful estimable fits
  with eligible computed conditional covariance give
  \code{inference_status = "conditional_supported"} and
  \code{inference_valid = TRUE} under the fitted model and selected-face
  assumptions. Apart from the compensation decision described in
  \code{\link{CIMEHR}}, loading, relevance and collinearity screens remain
  descriptive; their warnings or unavailability do not control eligibility.
  Existing fit and covariance checks still apply; regular inference remains
  distinct.
  No CI, curve SE, or bootstrap interval is suppressed solely because an
  exclusion restriction is absent. A covariate-name comparison does not
  determine inference availability. Zero-frailty nuisance fits retained by
  earlier versions must be refitted for the current identification checks.

  Observed-conditional predictions use the saved compensation basis and check
  whether the fitted model uniquely determines compensation at the requested
  covariates. A new observation random-link pattern is not itself a reason
  to stop. Predictions whose compensation can differ between equivalent
  fitted decompositions or coefficient representations stop with
  \code{cimehr_zero_frailty_identification_error} and reason
  \code{compensation_not_identified_by_fitted_design}.
  For example, compensation at \code{Z = 0.5} need not be determined by
  a fit that observed only binary \code{Z}. Population predictions do not
  include this observation-selection compensation term.
  Warnings raised during fitting (for example for
  \code{inference = "conditional"}) are stored in
  \code{diagnostics$warnings}. Visits exactly at the entry time are
  excluded as for \code{\link{CIMEHR}}.

  \code{CIMEHR_spline_HL} differs only in the posterior moments of
  \eqn{U_i}; see \code{\link{CIMEHR_HL}}. The corrected moments remain a
  local approximation; the sandwich does not remove approximation bias.
}
\value{
  \code{CIMEHR_spline} returns an object of class
  \code{c("cimehr_spline_fit", "cimehr_fit", "list")};
  \code{CIMEHR_spline_HL} returns class
  \code{c("cimehr_hl_fit", "cimehr_spline_fit", "cimehr_fit", "list")}.
  Both record \code{native_base_method = "cimehr_spline"} and
  \code{variant} (\code{"current"} or
  \code{"higher_order_gaussian_moments"}). Components include
  \item{method_key, method, estimand}{Method key (\code{"cimehr_spline"} or
    \code{"cimehr_spline_hl"}), label and target description.}
  \item{coefficients, se}{Stage-3 estimates and standard errors
    (\code{baseline.*}, \code{beta.*}, \code{trajectory.*},
    \code{theta.*}, or \code{compensation.*} at a zero frailty variance);
    \code{se} is \code{NULL} when
    \code{inference = "none"}.}
  \item{beta, trajectory, baseline_spline, theta}{The coefficient blocks.}
  \item{vcov}{Covariance of \code{coefficients} for the chosen
    \code{inference} (\code{NULL} for \code{"none"}).}
  \item{vcov_all}{With \code{inference = "stacked"}, the joint covariance
    of \code{gamma.*}, \code{sigma2_zeta}, the recording parameters and the
    Stage-3 coefficients; otherwise \code{NULL}.}
  \item{vcov_type, variance_type, variance_label}{Description of the
    covariance.}
  \item{fit}{Native fit with \code{spline_spec} (bases, knots,
    \code{reference_time}), \code{nuisance_summary} (Stage-1 and Stage-2
    estimates), \code{outcome_functionals} (\eqn{\omega_i} and
    \eqn{\kappa_i} by subject), \code{stacked_sandwich},
    \code{vcov_conditional}, \code{model_info} and, if requested,
    \code{nuisance_fit} and \code{stage3}.}
  \item{diagnostics}{List including \code{ok}, \code{converged},
    \code{identified}, \code{regular}, \code{conditional_inference_valid},
    \code{frailty_boundary}, \code{observation_variance_boundary},
    \code{inference_scope}, \code{stage3_design_rank},
    \code{stage3_scaled_condition_number}, \code{nuisance_fit_reused},
    \code{n_subjects}, \code{n_visits}, \code{n_outcomes},
    \code{n_visits_at_entry_excluded} and \code{warnings}; zero-frailty fits
    also include \code{identification_diagnostics},
    \code{identification_assessment}, \code{identification_status} and
    \code{identification_warning}.}
  \item{parameter_table, assumptions, call}{Outcome table (as from
    \code{\link{summary_outcome}}), assumptions and the matched call.}
  \code{\link{summary_visiting}} and \code{\link{summary_observation}}
  tabulate the Stage-1 and Stage-2 parameters.

  \code{predict} returns a data frame with columns \code{time} and
  \code{fit}, plus \code{profile} for separate profile predictions. With
  \code{se.fit = TRUE}, it adds \code{se}, \code{lower} and
  \code{upper} (normal-based pointwise limits). For an aggregated
  population curve, the weighted prediction design is formed before
  applying the joint coefficient covariance. Population predictions use
  the common interface described in \code{\link{meanfit}}. Observed
  predictions retain attributes \code{trajectory_type},
  \code{uncertainty_scope}, \code{inference_scope} and \code{regular};
  their uncertainty is conditional on the supplied \code{kappa}.
  \code{inference_scope} describes the covariance; when the fit's inference
  assessment (at zero frailty, or after a recorded fit or covariance
  failure) states a different scope, that scope is in attribute
  \code{inference_assessment_scope}.
}
\seealso{
  \code{\link{CIMEHR}}, \code{\link{fit_method}} (methods
  \code{"cimehr_spline"} and \code{"cimehr_spline_hl"}),
  \code{\link{summary_outcome}}.
}
\examples{
data("cimehr_example", package = "CIMEHR")
dat <- merge(cimehr_example$participants, cimehr_example$visits,
             by = "id", all.x = TRUE)
dat <- dat[dat$id <= 200, ]  # 200 of the 1,000 participants keeps the example fast
dat <- dat[order(dat$id, dat$time, na.last = TRUE), ]
rownames(dat) <- NULL

fit <- CIMEHR_spline(dat, covars_visit_XV = c("Z", "X"),
                     covars_outcome_fixed_XY = c("Z", "X"),
                     covars_outcome_random_link_ZY = "Z",
                     covars_obs_fixed_XO = c("Z", "X"),
                     covars_obs_random_link_ZO = "Z", censor_col = "C",
                     baseline_df = 5L, trajectory_covars = NULL,
                     boundary_knots = c(0, 60), reference_time = 0)
summary_outcome(fit)
profiles <- data.frame(Z = c(0, 1), X = c(0, 0))
predict(fit, newdata = profiles, times = c(12, 36, 60))
curve <- meanfit(fit, newdata = profiles, times = seq(0, 60, by = 12),
                 se.fit = TRUE)
summary(curve)

fit_hl <- CIMEHR_spline_HL(dat, covars_visit_XV = c("Z", "X"),
                           covars_outcome_fixed_XY = c("Z", "X"),
                           covars_outcome_random_link_ZY = "Z",
                           covars_obs_fixed_XO = c("Z", "X"),
                           covars_obs_random_link_ZO = "Z", censor_col = "C",
                           baseline_df = 5L, trajectory_covars = NULL,
                           boundary_knots = c(0, 60), reference_time = 0)
extract_coefficient(fit_hl, "Z")
}
\keyword{models}
\keyword{regression}
