\name{BEAST.asymptotic.calibrate}
\alias{BEAST.asymptotic.calibrate}
\alias{print.BEASTAsymptoticCalibration}
\title{Reusable Bivariate Asymptotic BEAST Calibration}
\usage{
BEAST.asymptotic.calibrate(n, dep = 3, subsample.percent = 3/16,
  B = 128, lambda = NULL, H = 1000, G = 10000, seed = NULL,
  ncores = 1)
\method{print}{BEASTAsymptoticCalibration}(x, ...)
}
\arguments{
\item{n}{Integer sample size, at least two.}
\item{dep}{Binary expansion depth, an integer from 1 through 5.}
\item{subsample.percent}{Subsample fraction in \eqn{(0,1]}. The effective size is the positive integer truncation \eqn{r=\lfloor n\,\mathtt{subsample.percent}\rfloor}.}
\item{B}{Positive integer number of subsamples, denoted by \eqn{m} in the paper.}
\item{lambda}{Finite nonnegative soft threshold. \code{NULL} uses the bivariate \code{BEAST} default \eqn{\sqrt{\log(2^{2\,\mathtt{dep}})/(8n)}}.}
\item{H}{Number of independent null replications for covariance estimation; integer at least two.}
\item{G}{Number of joint Gaussian draws; integer at least two.}
\item{seed}{Optional integer seed in \eqn{[0,2147483646]}. See reproducibility details.}
\item{ncores}{Must equal 1 for serial calibration.}
\item{x}{A calibration object.}
\item{...}{Reserved for print-method compatibility.}
}
\description{
Constructs a reusable joint Gaussian reference for BEAST testing independence
between two continuous variables.
}
\details{
Calibration uses all \eqn{q=(2^{\mathtt{dep}}-1)^2} cross interactions.
For each of \code{H} independent bivariate normal null samples, it computes
the full-sample interaction means and their average over \code{B} size-\eqn{r}
subsamples drawn without replacement. Marginal ranks are recomputed within
each subsample. The covariance of this joint vector, scaled by
\eqn{\sqrt{n-1}}, is estimated and negative eigenvalues are set to zero.

Joint Gaussian draws are soft-thresholded at \eqn{\sqrt{n-1}\lambda}.
The BEAST limit map divides the inner product of the thresholded full and
subsample vectors by the norm of the full vector, returning zero when that
norm is zero. The resulting \code{G0} draws are stored for testing at different
significance levels.

Both \code{n} and the effective \eqn{r} must be divisible by
\eqn{2^{\mathtt{dep}}}, with \eqn{r\ge 2}. A calibration can be saved with
\code{saveRDS()} and reused for bivariate datasets with the same sample size
and tuning parameters. For multivariate blocks, the covariance also depends
on within-block dependence, so this reusable calibration is bivariate only.

An explicit \code{seed} makes calibration reproducible and preserves the
caller's RNG state. With \code{seed=NULL}, a master seed is drawn from the
current RNG stream and recorded in the object. Calibration is serial.
}
\value{
An object of class \code{BEASTAsymptoticCalibration}, containing \code{n},
\code{dep}, \code{subsample.percent}, effective \code{r}, \code{B}, actual
\code{lambda}, \code{q}, \code{H}, \code{G}, the covariance \code{Gamma},
pre-clipping \code{Gamma.raw}, \code{G0}, nonzero fraction, minimum raw
eigenvalue, count of clipped eigenvalues, basis/configuration, RNG metadata,
package version, method identifier and bivariate continuous-margin scope.
}
\seealso{\code{\link{BEAST.asymptotic}}, \code{\link{BEAST}}}
\examples{
# Small counts for illustration; use larger H and G for inference.
cal <- BEAST.asymptotic.calibrate(32, dep = 2, subsample.percent = 1/2,
  B = 2, H = 8, G = 32, seed = 1)
cal
length(cal$G0)
}
