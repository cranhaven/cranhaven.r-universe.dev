\name{YonX}
\alias{YonX}
\title{Maximum Likelihood (ML) Shrinkage in Simple Linear Regression}
\description{
  Compute and display Normal-theory ML Shrinkage statistics when a y-Outcome Variable
  is regressed upon a SINGLE x-Variable (i.e. p = 1). This illustration is usefull in
  regression pedagogy. The OLS (BLUE) estimate is a scalar in these simple cases, so
  the MSE optimal Shrinkage factor, dMSE, is also a scalar less than +1 and greater
  than 0 when cor(y,x) differs from Zero. The corresponding m-Extent of Optimal
  Shrinkage is marked by the "purple" vertical dashed-line on all YonX() TRACE
  Diagnostics.
}
\usage{
  YonX(form, data, delmax = 0.999999)
}
\arguments{
  \item{form}{A regression formula [y ~ x] suitable for use with lm().}
  \item{data}{Data frame containing observations on both variables in the formula.}
  \item{delmax}{Maximum allowed value for Shrinkage delta-factor that is strictly less
   than 1. (default = 0.999999, which prints as 1 when rounded to fewer than 6 decimal
   places.)}
}
\details{Since only a single x-Variable is being used, these "simple" models are
  (technically) NOT "Ill-conditioned". Of course, the y-Outcome may be nearly
  multi-collinear with the given x-Variable, but this simply means that the model
  then has low "lack-of-fit". In fact, the OLS estimate can never have the "wrong"
  numerical sign in these simple p = 1 models!  Furthermore, since "risk" estimates
  are scalar-valued, no "exev" TRACE is routinely displayed; its content duplicates
  information in the "rmse" TRACE. Similarly, no "infd" TRACE is displayed because
  any "inferior direction" COSINE would be either: +1 ("upwards") when an estimate
  is decreasing, or -1 ("downwards") when an estimate is increasing. The m-Extent
  of shrinkage is varied from 0.000 to 1.000 in 1000 "steps" of size 0.001.
}
\value{ An output list object of class YonX:
  \item{data}{Name of the data.frame object specified as the second argument.}
  \item{form}{The regression formula specified as the first argument to YonX() must
    have only ONE right-hand-side X-variable in calls to YonX().}
  \item{p}{Number of X-variables MUST be p = 1 in YonX().}
  \item{n}{Number of complete observations after removal of all missing values.}
  \item{r2}{Numerical value of R-square goodness-of-fit statistic.}
  \item{s2}{Numerical value of the residual mean square estimate for error.}
  \item{prinstat}{Vector of five Principal Statistics: eigval, sv, b0, rho & tstat.} 
  \item{yxnam}{Character Names of "Y" and "X" data vectors.}
  \item{yvec}{"Y" vector of data values.}
  \item{xvec}{"X" vector of data values.}
  \item{coef}{Vector of Shrinkage regression Beta-coefficient estimates: delta * B0.}
  \item{rmse}{Vector of Relative MSE Risk estimates starting with the rmse of the OLS estimate.}
  \item{spat}{Vector of Shrinkage (multiplicative) delta-factors: 1.000 to 0.000 by -0.001.}
  \item{qrsk}{Vector of Quatratic Relative MSE Risk estimates with minimum at delta = dMSE.}
  \item{exev}{Vector of Excess Eigenvalues = Difference in MSE Risk: OLS minus GRR.}  
  \item{mlik}{Normal-theory Likelihood ...for Maximum Likelihood estimation of Shrinkage m-Extent.}
  \item{sext}{Listing of summary statistics for all M-extents-of-shrinkage.}
  \item{mUnr}{Unrestricted optimal m-Extent of Shrinkage from the dMSE estimate; mUnr = 1 - dMSE.}   
  \item{mClk}{Most Likely Observed m-Extent of Shrinkage: best multiple of (1/steps) <= 1.} 
  \item{minC}{Minimum Observed Value of CLIK Normal-theory -2*log(Likelihood-Ratio).}
  \item{minE}{Minimum Observed Value of EBAY (Empirical Bayes) criterion.}
  \item{minR}{Minimum Observed Value of RCOF (Random Coefficients) criterion.}  
  \item{minRR}{Minimum Relative Risk estimate.}
  \item{mRRm}{m-Extent of the Minimum Relative Risk estimate.}  
  \item{mReql}{m-Extent where the "qrsk" estimate is first >= the observed OLS RR at m = 0.}
  \item{Phi2ML}{Maximum Likelihood estimate of the Phi-Squared noncentrality parameter of the
    F-ratio for testing H: true beta-coefficient = zero.}
  \item{Phi2UB}{Unbiased Phi-Squared noncentrality estimate. This estimate can be negative.}	
  \item{dALT}{This Maximim Likelihood estimate of Optimal Shrinkage has serious Downward Bias.}
  \item{dMSE}{Best Estimate of Optimal Shrinkage Delta-factor from the "Correct Range" adjustment
    to the Unbiased Estimate of the NonCentrality of the F-ratio for testing Beta = 0.}  
}

\references{
  Obenchain RL. (1978) Good and Optimal Ridge Estimators. \emph{Annals of Statistics}
  \bold{6}, 1111-1121. \doi{10.1214/aos/1176344314}
  
  Obenchain RL. (2022) Efficient Generalized Ridge Regression. \emph{Open Statistics}
  \bold{3}: 1-18. \doi{10.1515/stat-2022-0108}
  
  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.1. \url{http://localcontrolstatistics.org}
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{correct.signs}} and \code{\link{MLtrue}}}
\examples{
  data(haldport)
  form <- heat ~ p4caf
  YXobj <- YonX(form, data=haldport)
  YXobj
  plot(YXobj)
}
\keyword{regression}
