\name{baseline_mean}
\alias{baseline_mean}
\alias{population_trajectory}
\alias{predict.cimehr_fit}
\alias{plot.cimehr_fit}
\title{Evaluate Baseline and Population Mean Curves}
\description{
Data-frame interfaces to the baseline and population mean predictions
used by \code{\link{meanfit}}. The data-frame prediction functions use an
existing fitted model without refitting it. Direct plotting can optionally
request the full patient bootstrap provided by \code{meanfit()}.
}
\usage{
baseline_mean(object, times = NULL, bandwidth = NULL,
              se.fit = FALSE, level = .95)

population_trajectory(object, newdata = NULL, times = NULL,
  bandwidth = NULL, aggregate = TRUE, weights = NULL,
  se.fit = FALSE, level = .95)

\method{predict}{cimehr_fit}(object, newdata = NULL, times = NULL,
  type = "population", aggregate = FALSE, weights = NULL,
  bandwidth = NULL, se.fit = FALSE, level = .95, ...)

\method{plot}{cimehr_fit}(x, type = c("trajectory", "baseline"),
  newdata = NULL, times = NULL, bandwidth = NULL, aggregate = is.null(newdata),
  weights = NULL, se.fit = FALSE, level = .95,
  xlab = "Time", ylab = NULL,
  bootstrap = FALSE, B = 50L, seed = NULL, workers = 1L, ...)
}
\arguments{
\item{object, x}{A supported fitted \code{cimehr_fit}; see
\code{\link{meanfit}} for the supported method keys.}
\item{newdata}{Numeric covariate profiles, a function returning profiles
at each requested time, or \code{NULL} for the fitted method's stored
reference profiles. See \code{\link{meanfit}}.}
\item{times}{Finite times within the stored follow-up domain, in the
fitted time units. Defaults to 101 equally spaced points in that domain.}
\item{bandwidth}{Positive local-linear bandwidth in the fitted time
units. Defaults to 0.2 times the domain width. Used only for
increment-based baseline recovery.}
\item{aggregate}{Average over profiles at each time. Defaults to
\code{TRUE} for \code{population_trajectory()}, \code{is.null(newdata)} for
direct plotting, and \code{FALSE} for \code{predict()}.}
\item{weights}{Nonnegative profile weights with positive sum, for
\code{aggregate = TRUE}; equal weighting is the default.}
\item{se.fit}{Request supported analytical curve standard errors and
pointwise Wald intervals. The default is \code{FALSE}. For direct plotting,
\code{bootstrap = TRUE} uses bootstrap uncertainty instead. See
\code{\link{meanfit}} for method-specific availability and interpretation.
An absent exclusion restriction does not by itself suppress supported
CIMEHR curve standard errors. Diagnostic concerns are retained in the
returned metadata without suppressing computable intervals.}
\item{level}{Confidence level strictly between zero and one.}
\item{type}{For \code{predict()}, only \code{"population"} is supported.
For direct plotting, \code{"trajectory"} draws the population mean and
\code{"baseline"} draws the common baseline mean.}
\item{xlab, ylab}{Axis labels; see \code{\link{meanfit}}.}
\item{bootstrap, B, seed, workers}{For direct plotting only: request an
optional patient-cluster bootstrap, set the number of samples (default
\code{50L}), an optional seed, and the worker count (default \code{1L}).
The default \code{bootstrap = FALSE} performs no bootstrap. These arguments
are passed to \code{\link{meanfit}}; they are not arguments of
\code{baseline_mean()}, \code{population_trajectory()}, or \code{predict()}.}
\item{\dots}{Reserved; additional arguments are rejected.}
}
\details{
\code{baseline_mean()} evaluates \eqn{\beta_0(t)} on the modeled outcome
scale. \code{population_trajectory()} adds the fitted outcome covariate
contribution with random effects averaged to their population mean.
\code{predict()} is a data-frame interface for separate profiles by
default. Use \code{meanfit()} to retain an explicit curve object for
subsequent summary and plotting.

Spline fits retain their existing \code{predict.cimehr_spline_fit} method,
including its \code{type = "observed"} option and its own defaults; see
\code{\link{CIMEHR_spline}}. Use \code{meanfit()},
\code{baseline_mean()}, or \code{population_trajectory()} for the common
mean-curve interface across all supported methods.

For bootstrap intervals, construct a curve with
\code{meanfit(fit, bootstrap = TRUE, B = 50L)} and then call \code{plot()}.
Alternatively, \code{plot(fit, bootstrap = TRUE, B = 50L)} constructs and
plots such a curve directly. Each bootstrap sample resamples whole subjects
and refits all stages. Intervals use the original curve plus or minus the
normal quantile times the bootstrap standard deviation at each point.
The number of finite bootstrap values may differ between points; use a
\code{meanfit} object to inspect the counts and failure diagnostics.

The default reference population, baseline recovery, covariate-path
requirements, and uncertainty restrictions are described in
\code{\link{meanfit}}. The functions do not automatically transform
predictions back from a modeled log outcome.
}
\value{
\code{baseline_mean()}, \code{population_trajectory()}, and \code{predict()}
return a data frame containing \code{time} and \code{fit}. Separate
profile predictions also contain \code{profile}. Supported uncertainty
requests add \code{se}, \code{lower}, and \code{upper}.
\code{plot()} draws a curve and invisibly returns a \pkg{ggplot2} object.
}
\seealso{\code{\link{meanfit}}, \code{\link{fit_method}}.}
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
times <- seq(0, 60, by = 12)
baseline_mean(fit, times = times)
population_trajectory(fit, times = times)
predict(fit, newdata = data.frame(Z = c(0, 1), X = c(0, 0)), times = times)

## plot() requires the suggested package ggplot2
if (requireNamespace("ggplot2", quietly = TRUE)) {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  plot(fit, type = "baseline", times = times)
  grDevices::dev.off()
  unlink(file)
}
}
\keyword{models}
\keyword{regression}
