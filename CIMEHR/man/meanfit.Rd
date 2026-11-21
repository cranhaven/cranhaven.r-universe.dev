\name{meanfit}
\alias{meanfit}
\alias{meanfit.cimehr_fit}
\alias{cimehr_meanfit}
\alias{print.cimehr_meanfit}
\alias{summary.cimehr_meanfit}
\alias{as.data.frame.cimehr_meanfit}
\alias{plot.cimehr_meanfit}
\title{Construct and Plot Fitted Longitudinal Mean Curves}
\description{
Constructs a fitted mean-curve object from a CIMEHR, Liang, MI/LI+Liang,
or EHRJoint model. The curve object can be summarized, converted to a data
frame, or plotted. An optional patient bootstrap refits the model to
estimate curve uncertainty; it is disabled by default. Once constructed,
the curve object can be summarized or plotted without further refitting.
}
\usage{
meanfit(object, ...)

\method{meanfit}{cimehr_fit}(object, newdata = NULL, times = NULL,
  type = c("population", "baseline"), aggregate = is.null(newdata),
  weights = NULL, bandwidth = NULL, se.fit = FALSE, conf.int = .95,
  bootstrap = FALSE, B = 50L, seed = NULL, workers = 1L, ...)

\method{print}{cimehr_meanfit}(x, ...)

\method{summary}{cimehr_meanfit}(object, ...)

\method{as.data.frame}{cimehr_meanfit}(x, row.names = NULL,
  optional = FALSE, ...)

\method{plot}{cimehr_meanfit}(x, conf.int = !is.null(x$conf.int),
  xlab = "Time", ylab = NULL, ...)
}
\arguments{
\item{object}{For \code{meanfit()}, a \code{cimehr_fit} returned by
\code{\link{fit_method}}. Supported method keys are \code{cimehr},
\code{cimehr_hl}, \code{cimehr_spline}, \code{cimehr_spline_hl},
\code{liang_visit}, \code{liang_marker}, \code{mi_liang}, \code{li_liang},
and \code{ehrjoint}. For \code{summary()}, a \code{cimehr_meanfit} object.}
\item{x}{A \code{cimehr_meanfit} object.}
\item{newdata}{A data frame with one row per covariate profile and the
numeric covariates or modifiers required by the fitted outcome design.
With \code{NULL}, uses one stored reference profile per subject in the
population retained by the fitted method. A function of one numeric time
may instead return a nonempty profile data frame at each requested time,
where the fitted model supports the specified covariate paths. Keep the
row ordering consistent across times for separate profile curves.
Not used for \code{type = "baseline"}.}
\item{times}{Finite prediction times in the fitted time units and within
the stored follow-up domain. The default is 101 equally spaced points in
that domain. Supplied times are sorted and duplicates removed.}
\item{type}{\code{"population"} for the outcome mean with random effects
averaged over their population distribution, or \code{"baseline"} for
the common baseline mean function \eqn{\beta_0(t)}.}
\item{aggregate}{Average predictions over covariate profiles at each time.
The default is \code{TRUE} when \code{newdata} is \code{NULL}, and
\code{FALSE} when profiles are supplied. With \code{FALSE}, returns one
curve per profile. Not used for baseline mean curves.}
\item{weights}{Optional finite, nonnegative profile weights with positive
sum, used only when \code{aggregate = TRUE}. Provide one weight per
profile; weights are normalized to sum to one. The default gives equal
weight to each profile. Not used for baseline mean curves.}
\item{bandwidth}{Positive local-linear smoothing bandwidth in the fitted
time units. The default is 0.2 times the width of the stored follow-up
domain. Used for increment-based baseline recovery; fitted spline or
parametric baseline bases are evaluated directly.}
\item{se.fit}{Compute analytical curve standard errors and pointwise Wald intervals.
The default is \code{FALSE}. Currently supported for the CIMEHR spline
methods when their joint coefficient covariance is available. Ignored when
\code{bootstrap = TRUE}, which uses bootstrap standard errors instead.
See Details.}
\item{conf.int}{For \code{meanfit()}, a confidence level strictly between
zero and one. For \code{plot()}, a logical indicating whether to draw
intervals already stored in the curve object; defaults to whether intervals
were requested when constructing the object.}
\item{bootstrap}{Use a patient-cluster bootstrap to estimate curve
standard errors and pointwise Wald intervals. The default, \code{FALSE},
performs no bootstrap. When \code{TRUE}, overrides analytical inference
requested through \code{se.fit}. Requires a fit with stored refitting data
and settings.}
\item{B}{Number of bootstrap samples, an integer of at least two.
The default is \code{50L}. Increase this, for example to \code{200L}, to
reduce Monte Carlo variability in the bootstrap standard errors.}
\item{seed}{Optional seed for the bootstrap.}
\item{workers}{Number of bootstrap workers, a positive integer. The
default is \code{1L}. Multicore execution is available on non-Windows
systems; Windows uses sequential execution with a warning when more than
one worker is requested.}
\item{row.names, optional}{Standard \code{as.data.frame} arguments.
\code{row.names}, when supplied, replaces the returned row names.}
\item{xlab, ylab}{Axis labels. The default vertical label is
\sQuote{Population-average outcome} or \sQuote{Baseline mean outcome}.}
\item{\dots}{Reserved for S3 methods. Additional arguments to
\code{meanfit()}, \code{summary()}, or \code{plot()} are rejected.}
}
\details{
For a supplied profile \eqn{x(t)}, the population mean is
\deqn{m(t;x)=\beta_0(t)+x(t)^\top\beta}{m(t; x) = beta_0(t) + x(t)' beta}
with the corresponding fitted spline design used by the spline methods.
Random effects are averaged to their population mean; subject-specific
empirical Bayes predictions are not used. All predictions remain on the
modeled outcome scale, including the log scale when a log-transformed
outcome was fitted.

With no \code{newdata}, the default averages one profile per subject in
the reference population retained by the fitted method. The reference
population can differ across methods, particularly after LI reconstruction
eligibility exclusions. Supply the same \code{newdata} and weights to
compare curves standardized to a common population. Stored profiles must
be suitable for the requested time path; CIMEHR fits with time-varying
modifiers that cannot be represented by stored subject profiles require
explicit \code{newdata}.

For ordinary CIMEHR and EHRJoint, the baseline is recovered by local-linear
smoothing of compensated baseline increments against visiting-baseline
increments. The cumulative numerator
\eqn{A(t)=\int_0^t\beta_0(u)\,d\Lambda_0(u)}{A(t), the integral of beta_0(u) with respect to Lambda_0(u)} is an intermediate quantity;
\code{type = "baseline"} returns the recovered \eqn{\beta_0(t)}.
Liang uses its stored outcome-event normalization or evaluates its fitted
parametric baseline basis. MI+Liang averages the baseline curves from its
completed-data fits. The CIMEHR spline methods evaluate the fitted
baseline spline directly. Local recovery returns \code{NA} where there
are fewer than three distinct usable event times in the bandwidth or the
local design is numerically singular.

For CIMEHR spline curves, \code{se.fit = TRUE} uses the stored joint
covariance of baseline-spline and outcome coefficients. For a prediction
design row \eqn{d(t)}, the variance is \eqn{d(t)Vd(t)^\top}{d(t) V d(t)'}.
For an aggregated curve, the weighted prediction design is formed before
applying this covariance. Intervals are pointwise, conditional on the
specified profiles, weights, and fitted spline basis.

Without a bootstrap, ordinary CIMEHR, Liang, MI/LI+Liang, and EHRJoint
provide point curves through this interface. Their outcome-coefficient
standard errors alone do not include the uncertainty in baseline recovery.
Newly fitted CIMEHR spline models retain an untruncated factor of their
participant influences. Analytical curve variances use that factor directly
to reduce cancellation between large covariance entries, retaining the fitted
conditional or all-stage stacked scope. This numerical calculation does not
establish identification or confidence-interval coverage.

Requesting \code{se.fit = TRUE} without a supported curve covariance raises
an error unless \code{bootstrap = TRUE} is also specified.

At zero visiting frailty, the absence of an exclusion restriction does not
by itself prevent supported curve standard errors or bootstrap intervals.
The fitted compensation must be identified, and the method-specific curve
covariance requirements above still apply. See \code{\link{CIMEHR}} for
conditional inference scope at a selected variance boundary. Returned curve
metadata distinguish numerical uncertainty availability from conditional
inference support; identification warnings do not remove computable limits.
At zero frailty, \code{conditional_supported} gives
\code{inference_valid = TRUE} for otherwise eligible computed uncertainty
under the fitted-model and selected-branch/face assumptions. Descriptive
identification warnings and missing descriptive diagnostics do not control
eligibility. Existing fitting, estimability and uncertainty-availability
requirements still apply. An explicitly recorded covariance failure also
prevents an analytical positive-frailty curve interval from being reported
as valid inference: computed numerical values remain, with failure status
and its reason. A full-refit bootstrap uses its own uncertainty calculation,
so failure of the parent analytical covariance alone does not invalidate
an otherwise available bootstrap interval; the parent failure remains visible
in the curve-data attributes \code{parent_covariance_failed} and
\code{parent_covariance_warning}, and in the inference reason.
For zero-frailty CIMEHR output, curve attributes include
\code{ci_available}, \code{inference_valid}, \code{inference_status},
\code{identification_status} and \code{identification_warning}; the
\code{meanfit} object also retains this metadata at the top level.

With \code{bootstrap = TRUE}, subjects are sampled with replacement from
the stored input data. Each sampled copy receives a new subject identifier
and retains its complete record, including a placeholder for a subject
without visits. Each bootstrap fit repeats all estimation stages and any
MI or LI operations using the stored evaluated settings. Supplied model
controls are retained, cached nuisance fits are discarded, and automatic
data-dependent defaults are recalculated from the bootstrap data. The
method's eligibility rules are applied in each refit.
For zero-frailty CIMEHR fits, bootstrap curves retain the
\code{parent_identification_assessment} separately. Their diagnostic support
is inherited from the parent fit and does not calibrate bootstrap coverage.
The bootstrap \code{inference_scope} describes refitting all stages, rather
than the selected-face analytical covariance used for Wald intervals.

For \code{newdata = NULL} and \code{aggregate = TRUE}, each bootstrap curve
averages over the reference population retained by that bootstrap fit.
With \code{aggregate = FALSE}, the original fitted reference profiles are
held fixed. Explicit \code{newdata}, including a profile-generating
function, is held fixed across bootstrap fits. Bootstrap requests with
\code{newdata = NULL} and non-\code{NULL} \code{weights} are rejected;
supply explicit profiles and their weights for weighted standardization.

The original prediction grid and smoothing bandwidth are used throughout.
At each time and profile, the bootstrap standard error is the sample
standard deviation of the finite bootstrap curve estimates, without
division by the square root of \code{B}. The pointwise Wald interval is
centered at the original fitted curve:
\deqn{\widehat m(t)\;\mathbin{\pm}\;
  z_{(1+\mathrm{conf.int})/2}\,\widehat{\mathrm{SE}}_{\mathrm{boot}}(t).}
The same calculation applies to a baseline mean curve. Failed fits and
nonfinite or unsupported curve points are recorded; failed attempts are
not replaced by new samples. A warning reports incomplete bootstrap
output. Each curve row reports its finite replicate count in
\code{n_boot}; fewer than two finite values produce \code{NA} for its
standard error and interval, while retaining the original point estimate.

Older fitted objects without the required prediction metadata may need to
be refitted with the updated package. Bootstrap additionally requires the
stored \code{refit} data and settings; older objects without this component
must be refitted. Plotting requires \pkg{ggplot2}; it draws stored intervals
by default when available. Use \code{plot(curve, conf.int = FALSE)} to hide
them without refitting.
}
\value{
\code{meanfit()} returns an object of class \code{cimehr_meanfit} with
components \code{data}, \code{time}, \code{method}, \code{method_key},
\code{type}, \code{aggregate}, \code{newdata}, \code{standardization},
\code{bandwidth}, \code{conf.int}, \code{outcome_scale}, and \code{call}.
Its \code{data} component contains \code{time} and \code{fit}, plus
\code{profile} for separate profile curves. Supported uncertainty requests
add \code{se}, \code{lower}, and \code{upper}. Bootstrap output also adds
\code{n_boot} to \code{data} and a \code{bootstrap} component containing
the \code{B}-by-curve-row matrix \code{replicates}, pointwise finite counts
\code{n_success}, the number of \code{successful_fits}, and failure
diagnostics, including the stage and reason for failed fits or incomplete
curves.

\code{summary()} and \code{as.data.frame()} return this curve data frame.
Choose prediction times when calling \code{meanfit()}.
\code{plot()} draws the curves and invisibly returns a \pkg{ggplot2}
object, which can be passed to \code{ggplot2::ggsave()} or modified with
additional ggplot layers. \code{print()} invisibly returns its input.
}
\seealso{
\code{\link{fit_method}}, \code{\link{baseline_mean}},
\code{\link{population_trajectory}}.
}
\examples{
data("cimehr_example", package = "CIMEHR")
d <- merge(cimehr_example$participants, cimehr_example$visits,
           by = "id", all.x = TRUE)
d <- d[d$id <= 200, ]  # 200 of the 1,000 participants keeps the example fast
d <- d[order(d$id, d$time, na.last = TRUE), ]
fit <- fit_method("cimehr", d, visit_covs = c("Z", "X"),
                  long_covs = c("Z", "X"), obs_fixed_covs = c("Z", "X"),
                  outcome_random_covs = "Z", obs_random_covs = "Z",
                  censor_col = "C")
profiles <- data.frame(Z = c(0, 1), X = c(0, 0))
times <- seq(0, 60, length.out = 13)

## Separate curves for two covariate profiles
curve <- meanfit(fit, newdata = profiles, times = times)
summary(curve)

## Baseline mean curve
as.data.frame(meanfit(fit, type = "baseline", times = times))

## Weighted standardization over the two profiles
as.data.frame(meanfit(fit, newdata = profiles, times = times,
                      aggregate = TRUE, weights = c(.4, .6)))

## Participant bootstrap. B = 5 only keeps the example fast; use B >= 50
## in practice.
fit_liang <- fit_method("liang_visit", d, visit_covs = c("Z", "X"),
                        long_covs = c("Z", "X"), outcome_random_covs = "Z",
                        censor_col = "C")
boot <- meanfit(fit_liang, newdata = profiles, bootstrap = TRUE,
                B = 5L, seed = 2026)
summary(boot)

## Plotting requires the suggested package ggplot2
if (requireNamespace("ggplot2", quietly = TRUE)) {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  plot(curve)
  plot(boot, conf.int = FALSE)
  grDevices::dev.off()
  unlink(file)
}
}
\keyword{models}
\keyword{regression}
