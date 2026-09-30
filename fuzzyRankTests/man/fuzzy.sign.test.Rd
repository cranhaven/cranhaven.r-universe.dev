\name{fuzzy.sign.test}
\alias{fuzzy.sign.test}
\alias{fuzzy.sign.ci}
\title{Fuzzy P-value, Decision, or Confidence Interval for the Sign Test}
\description{
  Calculate the fuzzy P-value, the fuzzy decision, or the fuzzy confidence
  interval associated with the sign test.
}
\usage{
fuzzy.sign.test(x, alternative = c("two.sided", "less", "greater"),
    mu = 0, tol = sqrt(.Machine$double.eps), alpha)
fuzzy.sign.ci(x, alternative = c("two.sided", "less", "greater"),
    tol = sqrt(.Machine$double.eps), conf.level = 0.95)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter.}
  \item{mu}{a number specifying the value of the median of the data
    distribution hypothesized under the null hypothesis.}
  \item{tol}{data values within \code{tol} of \code{mu} are considered
    equal to \code{mu}.}
  \item{alpha}{if missing, calculate the fuzzy P-value.  If provided,
    must be between zero and one, then calculate the fuzzy decision.}
  \item{conf.level}{confidence level.}
}
\details{
The fuzzy P-value is a random variable taking values in the interval
\eqn{(0, 1)}.  Its cumulative distribution function (CDF) is continuous
and piecewise linear.  Hence its probability density function (PDF) is
piecewise constant (a step function).  If \eqn{P} is the fuzzy P-value,
considered as a random variable, then the randomized test that rejects
the null hypothesis at significance level \eqn{\alpha}{alpha} when
\eqn{P < \alpha}{P < alpha} is an exact (randomized) test.

The fuzzy confidence interval is a fuzzy set,
whose \dQuote{membership function} is a function on the parameter space
taking values in the interval \eqn{[0, 1]}.  For rank tests, it is
piecewise constant (a step function).  In the regular case, it is one
on a narrow interval and some number between zero and one on the part
of some wider interval not contained in the narrower interval, zero
outside the wider interval, and the values at jumps are the average of left
and right limits.  In this case, the fuzzy interval can be easily interpreted
as a mixture of two confidence intervals (the narrow and the wide).
When no ties are possible, the values at the jumps do not matter. 
Otherwise, they do.  With ties, any or all of the intervals can be
degenerate, and the values at the jumps are not related to left and right
limits.  If \eqn{I(\mu)}{I(mu)} is the membership function of the fuzzy
confidence interval, then the randomized test that rejects the null
hypothesis that \eqn{\mu}{mu} is the true parameter value with probability
\eqn{1 - I(\mu)}{1 - I(mu)} is an exact (randomized) test.
}
\value{
  A list with class \code{"fuzzyranktest"} or class \code{"fuzzyrankci"}
  containing some of the
  following components:
  \item{knots}{the vector of points at which the CDF of the fuzzy P-value,
    which is continuous and piecewise linear, has discontinuous derivative
    or the vector of points at which the membership function of the fuzzy
    confidence is discontinuous and also \code{-Inf} or \code{Inf} if the
    fuzzy confidence interval is unbounded.}
  \item{values}{the values of the CDF of the fuzzy P-value at the knots.}
  \item{knot.values}{the values of the membership function of the fuzzy
    confidence interval at the knots.}
  \item{interval.values}{the values of the membership function of the fuzzy
    confidence interval between the knots.}
  \item{reject.prob}{if \code{alpha} is specified, the probability the
    randomized test rejects the null hypothesis, which is the same as
    the probability the fuzzy P-value is less than \code{alpha}.}
  \item{alpha}{the argument \code{alpha}.}
  \item{statistic}{the value of the test statistic with a name
    describing it.}
  \item{null.value}{the argument \code{mu}.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the type of test applied.}
  \item{data.name}{a character string giving the names of the data.}
  \item{conf.level}{the argument \code{conf.level}.}
  \item{tol}{the argument \code{tol}.}
}
\references{
  Geyer, C. J. and Meeden, G. D. (2005)
  Fuzzy and randomized confidence intervals and \eqn{P}-values
  (with discussion).
  \emph{Statistical Science}, \bold{20}, 358--387.
  \doi{10.1214/088342305000000340}.

  Thompson, E. A. and Geyer, C. J. (2007)
  Fuzzy \eqn{P}-values in latent variable problems.
  \emph{Biometrika}, \bold{94}, 49--60.
  \doi{10.1093/biomet/asm001}.
}
\seealso{
  \code{\link{plot.fuzzyrankci}},
  \code{\link{plot.fuzzyranktest}},
  \code{\link{print.fuzzyrankci}},
  \code{\link{print.fuzzyranktest}}.
}
\examples{
#### make up data ####
x <- c(-1.2, -0.7, 0.2, 0.2, 0.2, 0.3, 0.4, 0.9, 0.9, 1.0, 1.0,
    1.1, 1.5, 1.7, 1.9, 3.5, 5.1)
fuzzy.sign.test(x)
plot(fuzzy.sign.test(x))
fuzzy.sign.ci(x)
plot(fuzzy.sign.ci(x))
}
\keyword{htest}

