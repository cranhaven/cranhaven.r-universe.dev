\name{cimehr_fit-methods}
\alias{cimehr_fit-methods}
\alias{coef.cimehr_fit}
\alias{vcov.cimehr_fit}
\alias{confint.cimehr_fit}
\alias{summary.cimehr_fit}
\alias{print.cimehr_fit}
\title{Methods for Fitted CIMEHR and Comparison-Method Objects}
\description{
  Extract coefficients, covariance matrices and confidence intervals from,
  summarize, and print objects of class \code{"cimehr_fit"} returned by
  \code{\link{fit_method}}, the \link[=comparison_methods]{method wrappers}
  and \code{\link{CIMEHR}} and its variants.
}
\usage{
\method{coef}{cimehr_fit}(object, ...)

\method{vcov}{cimehr_fit}(object, all = FALSE, ...)

\method{confint}{cimehr_fit}(object, parm, level = 0.95,
    type = c("available", "regular", "conditional"), ...)

\method{summary}{cimehr_fit}(object, ...)

\method{print}{cimehr_fit}(x, ...)
}
\arguments{
  \item{object, x}{An object of class \code{"cimehr_fit"}.}
  \item{all}{Logical. If \code{FALSE} (default), the covariance of the
    outcome-model coefficients; if \code{TRUE}, the all-stage (joint)
    covariance matrix when the method provides one.}
  \item{parm}{Parameters for which intervals are computed, given as names
    from the \code{parameter} column of \code{\link{summary_outcome}(object)}
    or as row numbers of that table. All outcome parameters if missing.}
  \item{level}{Confidence level, strictly between 0 and 1.}
  \item{type}{For \code{confint}, \code{"available"} (default) returns
    available numerical intervals. Validity and scope are reported separately,
    including an explicit covariance-failure status if applicable.
    \code{"regular"} restricts output to regular inference;
    \code{"conditional"} uses an available conditional covariance, also
    returning regular intervals for regular fits.}
  \item{\dots}{Not used.}
}
\details{
  \code{summary}, \code{print} and \code{confint} work on the outcome
  table produced by \code{\link{summary_outcome}}. For CIMEHR and
  CIMEHR-HL (\code{cimehr} and \code{cimehr_hl}) this table contains only
  the outcome parameters (\code{beta.*}, \code{theta.*}, or identified
  \code{compensation.*} coordinates at zero frailty), whereas
  \code{coef} returns every estimated parameter; use
  \code{\link{coef_stage}} or \code{\link{summary_visiting}} and
  \code{\link{summary_observation}} for the other stages.

  \code{confint} uses \code{qt((1 + level) / 2, df)} with the \code{df}
  column of the outcome table (a normal quantile when \code{df = Inf}),
  and, by default, returns available numerical intervals with their inference
  metadata. A variance
  boundary does not by itself suppress a computable outcome-coefficient
  interval. \code{ci_available} reports that limits are available;
  \code{inference_valid} separately records support within the stated
  scope. At zero frailty, an eligible computed conditional covariance
  gives \code{conditional_supported} and \code{inference_valid = TRUE},
  subject to the existing fit and estimability checks. Descriptive loading,
  relevance and collinearity warnings, including missing descriptive
  diagnostics, do not change eligibility or remove available limits.
  Conditional support assumes the fitted model and selected branch/variance
  face; it is not a proof of coverage or population identification.
  \code{regular_inference_valid} separately identifies regular inference.
  A finite SE is reported by \code{se_available}. The stage
  summaries can additionally report numerical Wald limits in separate columns
  on an explicit \code{numerical_wald = TRUE} request; those limits do not
  change default intervals or make \code{inference_valid} true.
  See \code{\link{summary_visiting}}.
  If an object explicitly records a covariance failure, any existing finite
  numerical limits are retained but are marked
  \code{inference_status = "covariance_failure"} with
  \code{inference_valid = FALSE}; they are not certified regular intervals.
  Finite numerical values do not override a recorded failure.

  When the default output includes conditional intervals or when
  \code{type = "conditional"} is requested, the returned matrix carries
  \code{inference_type}, \code{interval_scope}, \code{inference_scope}
  and \code{inference_reason} attributes. Zero-frailty output also retains
  inference-validity and identification-diagnostic metadata. At a selected variance
  boundary, fixed-face intervals hold that branch or face fixed and omit
  uncertainty from selecting it. Their availability does not assert
  selection-adjusted coverage. \code{type = "regular"} retains only
  regular intervals. Nonfinite, unidentified or otherwise unavailable
  parameter covariances remain \code{NA}; no interval is fabricated for a
  zero variance component without an available variance estimate.

  With \code{all = TRUE}, \code{vcov} returns the first available of
  \code{object$vcov_all}, \code{object$inference$vcov_all} and
  \code{object$stacked_sandwich$vcov_joint}.
}
\value{
  \code{coef}: the named numeric vector \code{object$coefficients}.

  \code{vcov}: with \code{all = FALSE}, \code{object$vcov}, the covariance
  matrix of the outcome-model coefficients; with \code{all = TRUE}, the
  all-stage covariance matrix, or \code{NULL} if the method does not
  provide one.

  \code{confint}: a matrix with columns \code{lower} and \code{upper} and
  one row per selected outcome parameter.

  \code{summary}: a list with components \code{method} (method label),
  \code{coefficients} (the outcome table, as from
  \code{\link{summary_outcome}}), \code{diagnostics},
  \code{variance_label} (description of the variance estimator) and
  \code{assumptions}.

  \code{print}: prints the method label and the outcome table and, when the
  fit has zero-frailty identification warnings, a line listing their codes
  (the text is in \code{diagnostics$identification_warning}); returns
  \code{x} invisibly.
}
\seealso{
  \code{\link{fit_method}}, \code{\link{summary_outcome}},
  \code{\link{comparison_methods}}.
}
\examples{
## Simulated EHR-type data: one row per visit, 150 subjects
set.seed(2)
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

fit <- IIRR(dat, process = "visit", visit_covs = c("Z", "X"),
            long_covs = c("Z", "X"), censor_col = "C")
fit
coef(fit)
vcov(fit)
dim(vcov(fit, all = TRUE))   # outcome and visiting-process parameters
confint(fit)
confint(fit, parm = "Z", level = 0.90)
s <- summary(fit)
s$variance_label
s$coefficients[, c("parameter", "estimate", "se", "inference_valid")]

## A linear mixed model has no all-stage covariance
fit_lmm <- LMM(dat, visit_covs = c("Z", "X"), long_covs = c("Z", "X"))
is.null(vcov(fit_lmm, all = TRUE))
}
\keyword{methods}
\keyword{models}
