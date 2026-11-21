\name{CIMEHR}
\alias{CIMEHR}
\alias{CIMEHR_HL}
\title{CIMEHR Three-Stage Estimator for Informative Visiting and Recording}
\description{
  \code{CIMEHR} (method key \code{cimehr}) fits a longitudinal outcome
  model to electronic health record data in which both the timing of visits
  and whether the outcome is recorded at a visit may depend on a shared
  subject-level latent variable. It combines a shared-frailty visiting model
  with a Laplace approximation to the latent variable, a within-patient
  pairwise probit likelihood for recording, and a centred estimating
  equation for the outcome. \code{CIMEHR_HL} (method key \code{cimehr_hl})
  is the same estimator with higher-order corrected posterior moments of
  the latent variable.
}
\usage{
CIMEHR(data, id_col = "id", time_col = "time", y_col = "Y", r_col = "R",
    censor_col = "C", covars_visit_XV = NULL, covars_outcome_fixed_XY = NULL,
    covars_outcome_random_link_ZY = NULL, covars_obs_fixed_XO = NULL,
    covars_obs_random_link_ZO = NULL, time_precision = 0,
    outcome_fixed_structure = NULL, outcome_random_structure = NULL,
    outcome_random_intercept = TRUE, pairwise_control = list(),
    inference = c("stacked", "none"), derivative_step = 2e-05,
    retain_details = FALSE, entry_col = NULL)

CIMEHR_HL(data, id_col = "id", time_col = "time", y_col = "Y", r_col = "R",
    censor_col = "C", covars_visit_XV = NULL, covars_outcome_fixed_XY = NULL,
    covars_outcome_random_link_ZY = NULL, covars_obs_fixed_XO = NULL,
    covars_obs_random_link_ZO = NULL, time_precision = 0,
    outcome_fixed_structure = NULL, outcome_random_structure = NULL,
    outcome_random_intercept = TRUE, pairwise_control = list(),
    inference = c("stacked", "none"), derivative_step = 2e-05,
    retain_details = FALSE, entry_col = NULL)
}
\arguments{
  \item{data}{A data frame with one row per visit; see \sQuote{Data format}.}
  \item{id_col, time_col, y_col, r_col, censor_col}{Names of the subject
    identifier, visit time, outcome, recording indicator (1 if the outcome
    was recorded at the visit, 0 otherwise) and censoring-time columns.}
  \item{covars_visit_XV}{Character vector of subject-level covariates
    \eqn{X_V} of the visiting rate model. No intercept is used (the baseline
    rate absorbs it). \code{NULL} fits the visiting model without
    covariates.}
  \item{covars_outcome_fixed_XY}{Character vector of outcome covariates
    \eqn{X_Y} with fixed coefficients \eqn{\beta}; at least one is required.
    Each column must be constant within subject unless it is declared as a
    time-affine path in \code{outcome_fixed_structure}.}
  \item{covars_outcome_random_link_ZY}{Character vector of subject-level
    columns \eqn{Z_Y} that are multiplied by the latent-variable functional
    \eqn{\kappa_i} and enter the outcome model with coefficients
    \eqn{\theta}. Including the name of the time column adds a
    latent-linked time slope (\code{theta.time01}). \code{NULL} adds
    nothing beyond the intercept controlled by
    \code{outcome_random_intercept}.}
  \item{covars_obs_fixed_XO}{Character vector of subject-level covariates
    \eqn{X_O} of the recording model, added to an intercept that is always
    included. \code{NULL} gives an intercept-only design.}
  \item{covars_obs_random_link_ZO}{Character vector of subject-level columns
    \eqn{Z_O} of the recording model, added to an intercept that is always
    included. Each column carries a loading \eqn{\delta} on the latent
    variable and a subject-level random-effect variance
    \eqn{\sigma^2_q}{sigma2_q}.
    \code{NULL} gives an intercept only.}
  \item{time_precision}{Nonnegative number. If positive, visit, censoring and
    entry times are rounded to multiples of \code{time_precision} when the
    Stage-1 risk sets and baseline grid are formed. \code{0} (default) uses
    the exact times. An error is raised if rounding merges two visits of the
    same subject or moves a visit onto its entry time.}
  \item{outcome_fixed_structure}{\code{NULL} (default) or a list declaring
    time-affine outcome covariates; see \sQuote{Covariate structures}.}
  \item{outcome_random_structure}{\code{NULL} (default) or a list declaring
    the columns of \eqn{Z_Y}; see \sQuote{Covariate structures}.}
  \item{outcome_random_intercept}{Logical. If \code{TRUE} (default),
    \eqn{Z_Y} contains a constant column (coefficient
    \code{theta.Intercept}).}
  \item{pairwise_control}{Named list of Stage-2 optimisation settings:
    \code{n_starts} (number of starting values, 1 to 5; default 5),
    \code{score_tol} (tolerance on the scaled projected score used to declare
    convergence; default \code{1e-10}), \code{boundary_tol} (variance
    components below this value are refitted at zero; default \code{1e-7}),
    \code{identification_tol} (relative eigenvalue tolerance of the local
    identification check; default \code{1e-7}), \code{hessian_step}
    (finite-difference step of the Hessian; default \code{2e-5}),
    \code{iter.max}, \code{eval.max}, \code{rel.tol}, \code{x.tol} (passed to
    \code{\link[stats]{nlminb}}; defaults 250, 600, \code{1e-10},
    \code{1e-9}) and \code{optim_control} (a list merged into the
    \code{nlminb} control list).}
  \item{inference}{\code{"stacked"} (default) computes the all-stage stacked
    sandwich covariance; \code{"none"} returns point estimates only, with
    \code{NA} standard errors.}
  \item{derivative_step}{Relative step, strictly between 0 and 0.1, of the
    central finite differences used by the stacked sandwich.}
  \item{retain_details}{Logical. If \code{TRUE}, stage-level intermediate
    objects are kept in \code{fit$details} and, with
    \code{inference = "stacked"}, the patient-level influence functions in
    \code{fit$inference}.}
  \item{entry_col}{Optional name of a subject-level entry-time column
    \eqn{E_i} for delayed entry. \code{NULL} (default) means no left
    truncation.}
}
\details{
  \subsection{Data format}{
    One row per visit with the subject identifier, visit time, outcome
    (\code{NA} when not recorded), recording indicator, subject-level
    covariates and a censoring time \eqn{C_i} that is finite and constant
    within subject. A subject with no visits has exactly one row with
    \code{time}, \code{Y} and \code{R} all \code{NA}. All modelled
    covariates must be numeric; code factors with
    \code{\link[stats]{model.matrix}} first. \eqn{X_V}, \eqn{X_O} and
    \eqn{Z_O} must be constant within subject. Visits must lie in
    \eqn{[E_i, C_i]}; the visiting risk set is \eqn{(E_i, C_i]}. A visit
    exactly at \eqn{E_i} is not part of the visit process and is dropped
    automatically; the number dropped is returned as
    \code{diagnostics$n_visits_at_entry_excluded}. At least three subjects
    are required.
  }
  \subsection{Stage 1: visiting process}{
    Visits of subject \eqn{i} follow a counting process with rate
    \eqn{\eta_i \exp(X_{V,i}^\top\gamma)\,d\Lambda_0(t)}{eta_i
    exp(X_Vi' gamma) dLambda_0(t)} on \eqn{(E_i, C_i]}, where the frailty
    \eqn{\eta_i = \exp(\sigma_\zeta U_i - \sigma^2_\zeta/2)}{eta_i =
    exp(sigma_zeta U_i - sigma2_zeta/2)} is driven by a standard normal
    latent variable \eqn{U_i}. \eqn{\gamma} is estimated from the
    partial-likelihood score, \eqn{\Lambda_0} by a Breslow estimator, and
    \eqn{\sigma^2_\zeta}{sigma2_zeta} by a factorial-moment
    (method-of-moments) estimator, truncated at zero. The posterior of
    \eqn{U_i} given the visit count \eqn{m_i} and the expected count is
    approximated by a Gaussian (Laplace) distribution with the posterior
    mode as mean and the inverse curvature as variance.
  }
  \subsection{Stage 2: recording process}{
    Given a visit, the outcome is recorded when
    \eqn{X_{O,i}^\top\alpha + Z_{O,i}^\top(\delta U_i + q_i) + e_{ij} >
    0}{X_Oi' alpha + Z_Oi' (delta U_i + q_i) + e_ij > 0}, with
    \eqn{q_i \sim N(0, \mathrm{diag}(\sigma^2_q))}{q_i ~ N(0,
    diag(sigma2_q))} and \eqn{e_{ij} \sim N(0,1)}{e_ij ~ N(0, 1)}. With
    \eqn{U_i} replaced by its Stage-1 posterior, the parameters are
    estimated by maximising a within-patient pairwise probit likelihood
    (each pair of visits of a subject weighted \eqn{1/(m_i-1)}; subjects
    with one visit contribute a marginal probit term) under
    \eqn{\sigma^2_q \ge 0}{sigma2_q >= 0}. When the estimate of
    \eqn{\sigma^2_\zeta}{sigma2_zeta} is zero, see \sQuote{Zero visiting
    frailty} below. The fit yields, per subject, the recording probability
    \eqn{\omega_i} and the conditional mean
    \eqn{\kappa_i = E(U_i \mid \mathrm{recorded})}{kappa_i = E(U_i |
    recorded)}.
  }
  \subsection{Stage 3: outcome}{
    The outcome model is
    \eqn{E(Y_{ij}) = A(t_{ij}) + X_{Y,i}(t_{ij})^\top\beta +
      \kappa_i Z_{Y,i}(t_{ij})^\top\theta}{E(Y_ij) = A(t_ij) +
      X_Yi(t_ij)' beta + kappa_i Z_Yi(t_ij)' theta}
    with an unspecified baseline mean function \eqn{A(t)}. \eqn{\beta} and
    \eqn{\theta} solve an estimating equation over the recorded outcomes in
    which \eqn{A} is removed by risk-set centring; each subject at risk is
    weighted by \eqn{\omega_i m_i / \Lambda_{0,i}}{omega_i m_i / Lambda_0i},
    the estimated rate of recorded visits, where
    \eqn{\Lambda_{0,i}}{Lambda_0i} is the cumulative baseline
    visiting rate over the subject's risk window. No intercept is estimated.
  }
  \subsection{Inference}{
    With \code{inference = "stacked"}, the covariance is an all-stage
    patient-level stacked sandwich that propagates the estimation of
    \eqn{\gamma}, \eqn{\Lambda_0}, \eqn{\sigma^2_\zeta}{sigma2_zeta} and the
    Stage-2 parameters into the outcome coefficients; derivatives of the
    posterior moments and of \eqn{\omega_i}, \eqn{\kappa_i} are computed by
    finite differences with step \code{derivative_step}. The sandwich
    targets the Laplace-defined estimator and does not remove approximation
    bias. When the estimate of \eqn{\sigma^2_\zeta}{sigma2_zeta} or of some
    \eqn{\sigma^2_q}{sigma2_q} is zero, the covariance is conditional on the
    selected boundary face. Outcome summary tables and the default
    \code{confint} retain the finite SE and checked available interval.
    \code{ci_available} reports interval availability separately from
    \code{inference_valid}. At zero frailty, successful estimable fits with
    eligible computed conditional covariance and available intervals receive
    \code{inference_status = "conditional_supported"} and
    \code{inference_valid = TRUE}. Loading, relevance and collinearity
    thresholds are descriptive and do not control inference eligibility.
    Unavailable descriptive diagnostics alone do not force \code{NA}.
    Existing fitting, estimability and covariance-availability checks still
    apply. The intervals are retained, and
    \code{regular_inference_valid} remains \code{FALSE}.
    \code{TRUE} denotes support under the fitted model and selected-branch/
    face assumptions, not proof of population identification or coverage.
    The scope and reason are retained: fixed-face intervals omit
    face-selection uncertainty.
    \code{confint(fit, type = "regular")} restricts output to regular
    intervals. A zero model variance does not imply zero variance for an
    outcome coefficient; unavailable parameter covariances remain
    \code{NA}. Stage-1 visiting-regression and positive
    frailty-variance intervals use their own covariance checks and do not
    depend on downstream observation-variance boundaries. The selected-face
    calculation does not compute influences for fixed boundary variance
    coordinates, and their missing SEs are not replaced by zero. A finite
    supplied SE is retained, but an uncalibrated boundary interval is not
    included in the default limits. An explicit \code{numerical_wald = TRUE}
    request in a visiting or observation summary adds separate numerical
    limits without establishing boundary calibration or changing
    \code{inference_valid}. The reporting statuses distinguish unavailable
    inference from a fitting failure; see \code{\link{summary_visiting}}.
    When the frailty-variance estimate is positive but within
    \eqn{\sqrt{\log n}}{sqrt(log n)} standard errors of zero, the existing
    frailty-strength diagnostic flags possible weak identification:
    \code{diagnostics$frailty_weak_identification} is \code{TRUE}.
    This descriptive flag does not suppress an otherwise eligible computed
    covariance, SE, or Wald interval and does not override the actual fit,
    estimability and covariance checks.
    The threshold uses a divergence rate considered for identification-category
    selection by Andrews and Cheng (2012). This diagnostic does not implement
    their robust critical values or establish validity of CIMEHR Wald
    inference under weak identification.
    Stacked all-stage covariance remains the default; it is not disabled
    by these reporting distinctions.
  }
  \subsection{Zero visiting frailty}{
    When the estimate of \eqn{\sigma^2_\zeta}{sigma2_zeta} is exactly zero,
    the posterior of \eqn{U_i} is \eqn{N(0,1)} and the compensation formula
    is unchanged; a zero frailty variance alone does not remove the
    recording-outcome link. The check concerns whether the attainable
    compensation under the original model can be uniquely estimated and
    separated from the fixed outcome effects after profiling the baseline.
    It does not require every latent loading to be separately identified.
    Where an exact quotient is available, only identified coordinates are
    reported: total observation variances (\code{tau2.<pattern>}, for example
    \code{tau2.(Intercept)} for a scalar random intercept or \code{tau2.Z=0}
    and \code{tau2.Z=1} for binary \code{Z}) and compensation coefficients
    \code{compensation.<Z_Y column>|<Z_O pattern or column>} (for example
    \code{compensation.Intercept|Z=1}; the suffix is omitted for a scalar
    random intercept). These replace individually unidentified
    \code{delta.*}, \code{sigma2_q.*} and \code{theta.*} coordinates.
    Saturated \eqn{Z_O} patterns use exact 0/1 indicators. A nonlinear
    attainable product set is not expanded to an unrestricted linear space.

    A unique loading decomposition up to common sign can also support
    estimation in the original coordinates. The fitted covariance and the
    outcome design must pass the corresponding identification checks; their
    numerical rank does not prove regularity at the true parameter. When the
    three-stage procedure cannot uniquely estimate the compensation, fitting
    stops with \code{cimehr_zero_frailty_identification_error}, including
    \code{reason} and \code{stage}. The number of random-effect columns is
    not itself a rejection criterion.

    The compensation is estimated only when a relevant exclusion
    restriction exists: at least one observation-design direction lies
    outside the outcome design (\code{extra_rank >= 1}; see
    \sQuote{Zero-frailty identification diagnostics}) and its relevance
    statistic exceeds \eqn{\sqrt{\log n}}{sqrt(log(n))}. The statistic
    uses the Stage-2 sandwich covariance of \eqn{\hat\alpha} (inverse
    pairwise Hessian and centered subject scores), which at zero frailty
    equals its stacked covariance and is computed for every
    \code{inference} option. When this exclusion-relevance criterion is not
    met, the zero-frailty estimation procedure includes a zero-compensation
    restriction (\eqn{c = 0}), the Stage-3 path also used when the fitted
    total observation variance is zero. This restriction does not establish
    the absence of informative observation; the remaining coefficients,
    stacked SEs and CIs are reported and are interpreted under it, and one
    \code{cimehr_identification_warning} with code
    \code{compensation_omitted} is signalled. A covariate that affects
    recording but not the outcome can strengthen identification.
    \code{diagnostics$zero_frailty_compensation} records \code{status}
    (\code{"estimated"}, \code{"omitted_no_relevant_exclusion"} or
    \code{"omitted_zero_total_observation_variance"}), \code{reason}
    (for \code{"omitted_no_relevant_exclusion"}:
    \code{"no_excluded_direction"}, \code{"weak_relevance"} or
    \code{"relevance_not_assessed"}; otherwise \code{NA}), \code{extra_rank}
    and the relevance statistic. The software does not infer an exclusion
    restriction from covariate names.

    Default stacked covariance propagates all remaining identifiable
    nuisance-stage quantities and their cross-stage covariance, including
    uncertainty in observation probabilities, compensation multipliers and
    centering weights. At zero frailty this calculation is conditional on
    the selected zero-frailty branch and variance face; it does not assert
    unconditional or selection-adjusted coverage. Computable covariance,
    SEs and CIs within that scope are retained.

  }
  \subsection{Zero-frailty identification diagnostics}{
    \code{diagnostics$identification_diagnostics} contains design and
    fitted-information diagnostics. Their exclusion and relevance
    definitions also give the criterion that decides whether the
    compensation is estimated or restricted to zero (see
    \sQuote{Zero visiting frailty}); apart from that decision, these
    diagnostics do not change the estimator, covariance, or interval limits.
    The list contains:
    \describe{
      \item{exclusion}{\code{extra_rank} is the dimension of the observation
        fixed-design space outside the intercept and outcome fixed/time/
        trajectory modifier space, among subjects with visits. The comparison
        uses column spaces, not variable names. An additional direction does
        not establish a valid exclusion restriction.}
      \item{relevance}{\code{statistic} is
        \eqn{\sqrt{b^T V_b^{-1}b/r}}{sqrt(b' V_b^(-1) b / r)}, where
        \eqn{b} is the fitted observation predictor projected onto the
        \eqn{r} additional directions and \eqn{V_b} is its covariance
        obtained from the available stacked covariance of \eqn{\hat\alpha}.
        The compensation decision uses the same definition with the Stage-2
        sandwich covariance; a value at or below
        \eqn{\sqrt{\log n}}{sqrt(log(n))} imposes the zero-compensation
        restriction. Here \eqn{n} counts eligible independent subjects.
        This is an Andrews--Cheng-inspired diagnostic, not their calibrated
        robust critical-value procedure.}
      \item{compensation}{\code{minimum_residual_fraction} is the smallest
        residual norm fraction of a compensation direction after projection
        on the outcome design. A value below the descriptive threshold
        \code{0.1} gives a \code{near_collinear} warning.
        \code{condition_number} is the condition number of the
        column-scaled combined design. The ordinary estimator uses its
        baseline-centered design, whereas the spline estimator explicitly
        includes baseline and trajectory columns.}
      \item{loading}{For a loading decomposition locally unique up to common
        sign, \code{third_largest_signal} is the third-largest
        \eqn{|\hat\delta_j|/\mathrm{SE}(\hat\delta_j)}{abs(delta_j)/SE(delta_j)}.
        A value at or below \eqn{\sqrt{\log n}}{sqrt(log(n))} gives a
        \code{weak_loading} warning. Numerical uniqueness and a large signal
        do not verify the population loading structure; the
        \code{loading_regularity_not_verified} warning remains.}
      \item{warning_codes, warnings, status}{Codes and explanations for the
        applicable diagnostics. An unavailable diagnostic is
        \code{not_assessed}. \code{no_warning_detected} does not certify
        identification or coverage.}
      \item{coverage_status}{\code{not_established_by_diagnostics}; the
        checks do not establish nominal confidence-interval coverage.}
    }
    The warning condition is \code{cimehr_identification_warning}. It does
    not suppress a computable covariance, SE, or CI. For the loading,
    relevance and compensation-collinearity screens, crossing their warning
    thresholds, or lacking the information to assess them, does not
    automatically change \code{inference_valid}; the exclusion-relevance
    criterion decides only whether the compensation is estimated. The same
    applies to the \code{loading_regularity_not_verified} reminder.
    Conditional support instead requires the existing fit, estimability and
    covariance-eligibility checks. It remains conditional on
    the model and selected branch/face assumptions and does not prove
    population identification or nominal coverage. Visiting parameters retain
    their own covariance checks. \code{diagnostics$identification_assessment} stores
    the overall outcome assessment with \code{valid}, \code{status},
    \code{reason} and \code{scope}. Parameter summaries and curves distinguish
    numerical availability from conditional inference support; see
    \code{\link{summary_outcome}}.
  }
  \subsection{Covariate structures}{
    \code{outcome_fixed_structure} is a list with elements \code{columns},
    \code{time_origin} (default 0) and \code{time_scale} (default 1).
    \code{columns} is a named list with one data frame per time-varying
    covariate in \code{covars_outcome_fixed_XY}, with columns
    \code{modifier} (a subject-constant column name, or
    \code{"(Intercept)"} for 1) and \code{power} (0 or 1). The covariate
    path is the sum over rows of \code{modifier} times
    \eqn{((t - \mathrm{origin})/\mathrm{scale})^{\mathrm{power}}}{((t -
    origin)/scale)^power}, and it must reproduce the observed column at
    every visit. Covariates not listed
    are treated as constant within subject. For example,
    \code{list(columns = list(age = data.frame(modifier = c("age0",
    "(Intercept)"), power = c(0, 1))))} declares
    \code{age = age0 + time}.

    \code{outcome_random_structure} is either a list with \code{static_covs}
    (subject-constant columns of \eqn{Z_Y}), \code{time_covs} (columns
    multiplied by \eqn{(t - \mathrm{origin})/\mathrm{scale}}{(t - origin)/scale};
    \code{"(Intercept)"} gives a pure time slope named \code{time01}),
    \code{time_origin} and \code{time_scale}, or a list with \code{columns},
    \code{time_origin} and \code{time_scale} in the format of
    \code{outcome_fixed_structure}. When \code{NULL}, \eqn{Z_Y} consists of
    the intercept (if \code{outcome_random_intercept}), the non-time columns
    of \code{covars_outcome_random_link_ZY}, and a time slope if the time
    column is listed there.
  }
  \subsection{CIMEHR-HL}{
    \code{CIMEHR_HL} replaces the Laplace posterior mean and variance of
    \eqn{U_i} by moments corrected with the third and fourth derivatives of
    the log posterior density (Gaussian moment closure). Stages 2 and 3 and
    the sandwich are unchanged. The corrected moments remain a local
    approximation; the sandwich does not remove approximation bias.
  }
  \code{CIMEHR(...)} and \code{CIMEHR_HL(...)} call
  \code{\link{fit_method}} with method \code{"cimehr"} and
  \code{"cimehr_hl"}. Through \code{fit_method}, the options
  \code{time_precision}, \code{pairwise_control}, \code{inference},
  \code{derivative_step} and \code{retain_details} are passed in
  \code{controls}. Note that
  \code{fit_method} uses the visiting covariates as the default recording
  covariates, whereas these wrappers use an intercept-only recording design
  when \code{covars_obs_fixed_XO} is \code{NULL}.
}
\value{
  An object of class \code{c("cimehr_fit", "list")} (\code{CIMEHR}) or
  \code{c("cimehr_hl_fit", "cimehr_fit", "list")} (\code{CIMEHR_HL}), a list
  with components including
  \item{method_key, method, estimand}{Method key (\code{"cimehr"} or
    \code{"cimehr_hl"}), label (\code{"CIMEHR"} or \code{"CIMEHR-HL"})
    and a description of the target.}
  \item{coefficients}{Named vector of all estimates: outcome coefficients
    \code{beta.*} and \code{theta.*}, visiting coefficients \code{gamma.*},
    recording parameters \code{alpha.*}, \code{delta.*} and
    \code{sigma2_q.*}, and the frailty variance \code{sigma2_zeta}. At a
    zero frailty variance \code{compensation.*} and \code{tau2.*} replace
    \code{theta.*}, \code{delta.*} and \code{sigma2_q.*}; see
    \sQuote{Zero visiting frailty}.}
  \item{se}{Standard errors matching \code{coefficients} (\code{NA} when
    \code{inference = "none"} or unavailable at a boundary).}
  \item{vcov}{Covariance matrix of the outcome coefficients
    (\code{beta.*}, \code{theta.*}).}
  \item{vcov_all}{Joint stacked covariance of all coefficients, or
    \code{NULL} when \code{inference = "none"}. Rows and columns of
    boundary parameters are \code{NA}.}
  \item{vcov_type, variance_type, variance_label}{Description of the
    covariance.}
  \item{fit}{The native fit, with components \code{visiting_process}
    (\eqn{\gamma}, baseline increments \code{Lambda0_hat},
    \eqn{\sigma^2_\zeta}{sigma2_zeta}, posterior means \code{mu_Ui} and variances
    \code{sigma2_Ui}), \code{observation_process} (\eqn{\alpha},
    \eqn{\delta}, \eqn{\sigma^2_q}{sigma2_q} and their covariance),
    \code{longitudinal_outcome} (\eqn{\beta}, \eqn{\theta},
    \eqn{\omega_i}, \eqn{\kappa_i}, estimated baseline \eqn{A(t)}),
    \code{inference} (stacked sandwich) and, with
    \code{retain_details = TRUE}, \code{details}.}
  \item{diagnostics}{List including \code{ok}, \code{converged},
    \code{identified}, \code{regular}, \code{frailty_boundary},
    \code{observation_variance_boundary}, \code{inference_scope},
    \code{n_subjects}, \code{n_visits}, \code{n_outcomes},
    \code{n_visits_at_entry_excluded}, optimiser diagnostics
    (\code{visiting}, \code{pairwise}), \code{warnings} and
    \code{elapsed_seconds}; at a zero frailty variance also
    \code{compensation_identification}, \code{identification_diagnostics},
    \code{identification_assessment}, \code{identification_status} and
    \code{identification_warning}.}
  \item{parameter_table}{Outcome coefficient table as returned by
    \code{\link{summary_outcome}}.}
  \item{assumptions, call}{Model assumptions and the matched call.}
  The object also records \code{native_base_method = "cimehr"} and
  \code{variant}, which is \code{"current"} for \code{CIMEHR} and
  \code{"higher_order_gaussian_moments"} for \code{CIMEHR_HL}. Methods for
  \code{coef}, \code{vcov} (\code{all = TRUE} gives \code{vcov_all}),
  \code{confint}, \code{summary} and \code{print} are available (see
  \code{\link{coef.cimehr_fit}}); \code{\link{summary_visiting}} and \code{\link{summary_observation}}
  tabulate the Stage-1 and Stage-2 parameters.
}
\references{
  Andrews, D. W. K. and Cheng, X. (2012). Estimation and inference with
  weak, semi-strong, and strong identification. \emph{Econometrica},
  \bold{80}, 2153--2211. \doi{10.3982/ECTA9456}.
}
\seealso{
  \code{\link{CIMEHR_spline}} for the spline-baseline version,
  \code{\link{fit_method}}, \code{\link{available_comparison_methods}},
  \code{\link{summary_outcome}}.
}
\examples{
data("cimehr_example", package = "CIMEHR")
dat <- merge(cimehr_example$participants, cimehr_example$visits,
             by = "id", all.x = TRUE)
dat <- dat[dat$id <= 200, ]  # 200 of the 1,000 participants keeps the example fast
dat <- dat[order(dat$id, dat$time, na.last = TRUE), ]
rownames(dat) <- NULL

fit <- CIMEHR(dat, covars_visit_XV = c("Z", "X"),
              covars_outcome_fixed_XY = c("Z", "X"),
              covars_outcome_random_link_ZY = "Z",
              covars_obs_fixed_XO = c("Z", "X"),
              covars_obs_random_link_ZO = "Z", censor_col = "C")
summary_outcome(fit)
summary_visiting(fit)
summary_observation(fit)
confint(fit)

fit_hl <- CIMEHR_HL(dat, covars_visit_XV = c("Z", "X"),
                    covars_outcome_fixed_XY = c("Z", "X"),
                    covars_outcome_random_link_ZY = "Z",
                    covars_obs_fixed_XO = c("Z", "X"),
                    covars_obs_random_link_ZO = "Z", censor_col = "C")
extract_coefficient(fit_hl, "Z")
}
\keyword{models}
\keyword{regression}
