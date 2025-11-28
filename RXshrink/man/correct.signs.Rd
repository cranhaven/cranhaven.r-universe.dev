\name{correct.signs}
\alias{correct.signs}
\title{Normal-Theory Maximum Likelihood Estimation of Beta Coefficients with "Correct" Signs}
\description{
  Obenchain(1978) discussed the risk of linear generalized ridge estimators in individual directions
  within p-dimensional X-space. While shrinkage to ZERO is clearly optimal for all directions
  strictly ORTHOGONAL to the true BETA, he showed that optimal shrinkage in the UNKNOWN direction
  PARALLEL to the true BETA is possible. This optimal BETA estimate is of the form k * X'y, where
  k is the positive scalar given in equation (4.2), page 1118. The correct.signs() function computes
  this estimate, B(=), that uses GRR delta-shrinkage factors proportional to X-matrix eigenvalues.
}
\usage{
  correct.signs(form, data)
}
\arguments{
  \item{form}{A regression formula [y~x1+x2+...] suitable for use with lm().}
  \item{data}{Data frame containing observations on all variables in the formula.}
}
\details{Ill-conditioned (nearly multi-collinear) regression models can produce Ordinary Least
  Squares estimates with numerical signs that differ from those of the X'y vector. This is disturbing
  because X'y contains the sample \bold{correlations} between the X-predictor variables and
  y-response variable. After all, these variables have been "centered" by subtracting off their
  mean values and rescaled to vectors of length one.  Besides displaying OLS estimates, the
  correct.signs() function also displays the "correlation form" of X'y, the estimated delta-shrinkage
  factors, and the k-rescaled beta-coefficients. Finally, the "Bfit" vector of estimates proportional
  to B(=) is displayed that minimizes the restricted Residual Sum-of-Squares. This restricted RSS of
  Bfit cannot, of course, be less than the RSS of OLS, but it can be MUCH less that the RSS of B(=)
  whenever B(=) shrinkage appears excessive.
}
\value{ An output list object of class "correct.signs":
  \item{data}{Name of the data.frame object specified as the second argument.}
  \item{form}{The regression formula specified as the first argument.}
  \item{p}{Number of regression predictor variables.}
  \item{n}{Number of complete observations after removal of all missing values.}
  \item{r2}{Numerical value of R-square goodness-of-fit statistic.}
  \item{s2}{Numerical value of the residual mean square estimate of error.}
  \item{prinstat}{Listing of principal statistics (p by 5) from qm.ridge().}
  \item{kpb}{Maximum likelihood estimate of k-factor in equation (4.2) of Obenchain(1978).}
  \item{bmf}{Rescaling factor for B(=) to minimize the Residual Sum-of-Squares.}
  \item{signs}{Listing of five Beta coefficient statistics (p by 5): OLS, X'y, Delta, B(=) and Bfit.}
  \item{loff}{Lack-of-Fit statistics: Residual Sum-of-Squares for OLS, X'y, B(=) and Bfit.}
  \item{sqcor}{Squared Correlation between the y-vector and its predicted values. The two values
    displayed are for OLS predictions or for predictions using Bfit, X'y or B(=). These two values
    are the familiar R^2 coefficients of determination for OLS and Bfit.}
}
\references{
  Obenchain RL. (1978) Good and Optimal Ridge Estimators. \emph{Annals of Statistics}
  \bold{6}, 1111-1121. \doi{10.1214/aos/1176344314}
  
  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.2. \url{http://localcontrolstatistics.org}
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{eff.ridge}}, \code{\link{qm.ridge}} and \code{\link{MLtrue}}.}
\examples{
  data(longley2)
  form <- GNP~GNP.deflator+Unemployed+Armed.Forces+Population+Year+Employed
  rxcsobj <- correct.signs(form, data=longley2)
  rxcsobj
  str(rxcsobj)
}
\keyword{regression}
