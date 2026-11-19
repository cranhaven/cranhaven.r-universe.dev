\name{BEAST.asymptotic}
\alias{BEAST.asymptotic}
\alias{print.BEASTAsymptoticResult}
\title{Bivariate Asymptotic BEAST Test}
\usage{
BEAST.asymptotic(X, calibration, alpha = 0.05, seed = NULL)
\method{print}{BEASTAsymptoticResult}(x, ...)
}
\arguments{
\item{X}{Numeric matrix or all-numeric data frame with exactly two columns,
finite observations, no missing values, and nonconstant margins.}
\item{calibration}{An object from \code{BEAST.asymptotic.calibrate}.
Its sample size must equal \code{nrow(X)}. Tuning parameters are taken from
this object.}
\item{alpha}{Significance level strictly between zero and one.}
\item{seed}{Optional integer seed in \eqn{[0,2147483646]} for the observed
statistic's subsampling. An explicit seed preserves the caller's RNG state;
\code{NULL} uses the current RNG stream.}
\item{x}{A test result.}
\item{...}{Reserved for print-method compatibility.}
}
\description{
Tests independence between two continuous variables using a stored calibration
based on the joint Gaussian limit of full-sample and internally reranked
subsample interactions.
}
\details{
The function calls \code{BEAST(method="stat")} with the tuning parameters
stored in \code{calibration} and compares \eqn{\sqrt{n-1}B_\lambda} with its
\code{G0} draws.

The p-value is \code{mean(calibration$G0 >= Scaled.Statistic)}. The critical
value is \code{sort(calibration$G0)[ceiling((1-alpha)*G)]}, and rejection uses
\code{Scaled.Statistic > critical.value}. At atoms, this strict decision can
differ from comparing the empirical p-value with \code{alpha}.

The dataset must have two columns and the calibration's sample size.
Marginal ties produce a warning; permutation calibration is recommended for
tied data. A nonpositive critical value also requires permutation calibration
and produces an error. A nonfinite BEAST statistic produces an error.

The same calibration can be reused across datasets with matching sample size
and tuning parameters, and at different significance levels. See
\code{\link{BEAST.asymptotic.calibrate}} for its construction.
}
\value{
An object of class \code{BEASTAsymptoticResult}, with \code{BEAST.Statistic},
\code{Scaled.Statistic}, \code{p.value}, \code{critical.value}, logical
\code{reject}, \code{alpha}, \code{Interaction} from the BEAST statistic,
calibration configuration/method summary, tie indicator and optional seed.
}
\seealso{\code{\link{BEAST.asymptotic.calibrate}}, \code{\link{BEAST}}}
\examples{
# Small counts for illustration; use larger H and G for inference.
i <- seq_len(32)
X1 <- cbind(i/33, i/33 + 0.08*sin(i))
X2 <- cbind(i/33, -i/33 + 0.08*cos(i))
cal <- BEAST.asymptotic.calibrate(32, dep=2, subsample.percent=1/2,
  B=2, lambda=0, H=12, G=64, seed=1)
BEAST.asymptotic(X1, cal, seed=11)
BEAST.asymptotic(X2, cal, seed=12)
}
