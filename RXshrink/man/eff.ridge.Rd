\name{eff.ridge}
\alias{eff.ridge}
\title{Efficient Maximum Likelihood (ML) Shrinkage via the Shortest Piecewise Linear-Spline PATH}
\description{
  Compute and display TRACEs for the p-paramater Shrinkage PATH passing through the
  (classical) Normal-theory Maximum Likelihood (ML) point-estimate of the Beta coefficient
  vector. The m-Extent of overall Optimal Shrinkage corresponding to this solution occurs
  at the only "interior" Knot on the Shrinkage Path and is also marked by the vertical
  dashed-line drawn on all 5-types of eff.ridge TRACE displays.
}
\usage{
  eff.ridge(form, data, rscale = 1, steps = 20, \dots)
}
\arguments{
  \item{form}{A regression formula [y~x1+x2+...+xp] suitable for use with lm().}
  \item{data}{data.frame containing observations on all variables in the formula.}
  \item{rscale}{One of three possible choices (0, 1 or 2) for "rescaling" of variables
   (after being "centered") to remove all "non-essential" ill-conditioning: 0 implies no
   rescaling; 1 implies divide each variable by its standard error; 2 implies rescale as
   in option 1 but re-express answers as in option 0.}
  \item{steps}{Number of equally spaced values per unit change along the horizontal
   M-extent-of-shrinkage axis for estimates to be calculated and displayed in TRACES
   (default = 20.)}
  \item{\dots}{Optional argument(s)}
}
\details{Ill-conditioned and/or nearly multi-collinear regression models are unlikely to
  produce Ordinary Least Squares (OLS) regression coefficient estimates that are very
  close, numerically, to their unknown true values.  Specifically, OLS estimates can have
  unreasonable relative magnitudes or "wrong" numerical signs when the number of x-variables
  is 2 or more. Shrunken (Generalized Ridge Regression) estimates chosen to maximize their
  likelihood of reducing Mean Squared Error (MSE) Risk (expected Squared Error Loss) can be
  more stable and reasonable, numerically.  On the other hand, because only OLS estimates
  are guaranteed to be minimax when risk is matrix valued (truly multivariate), no guarantee
  of an actual reduction in MSE Risk is necessarily associated with shrinkage. 
}
\value{ An output list object of class eff.ridge:
  \item{data}{Name of the data.frame object specified as the second argument.}
  \item{form}{The regression formula is the first argument.}
  \item{p}{Number of regression x-predictor variables.}
  \item{n}{Number of complete observations after removal of all missing values.}
  \item{r2}{Numerical value of R-squared: proportion of variance explained.}
  \item{s2}{Numerical value of the residual mean square estimate of error.}
  \item{prinstat}{Listing of 5 summary statistics for each of p-Principal Axes.}  
  \item{rscale}{Variable re-scaling code of 0, 1 or 2 used in calculations.}
  \item{data}{The data.frame containing all variables listed in the formula.}
  \item{gmat}{Orthogonal Matrix of Direction Cosines for Principal Axes.}
  \item{coef}{Matrix of shrinkage-ridge regression coefficient estimates.}
  \item{rmse}{Matrix of MSE risk estimates for fitted coefficients.}
  \item{exev}{Matrix of excess MSE eigenvalues (ordinary least squares minus ridge.)}
  \item{infd}{Matrix of direction cosines for the estimated inferior direction, if any.}
  \item{spat}{Matrix of shrinkage pattern multiplicative delta-factors.}
  \item{mlik}{Listing of criteria for maximum likelihood selection of an m-Extent for Shrinkage.}
  \item{sext}{Listing of summary statistics for all M-extents-of-shrinkage.}
  \item{mStar}{Optimal m-Extent of Shrinkage with delta[j] = dMSE[j] on TRACE displays.}
  \item{mMSE}{Minimum MSE Risk estimate.}  
  \item{mClk}{Most Likely Observed Extent of Shrinkage: best multiple of (1/steps) <= p.} 
  \item{minC}{Minimum Observed Value of Normal-theory -2*log(Likelihood-Ratio).}
  \item{dMSE}{Most Likely to be Optimal-values for Shrinkage: dMSE[j] for j in [1:p].}  
}
\references{
  Thompson JR. (1968) Some shrinkage techniques for estimating the mean. \emph{Journal
  of the American Statistical Association} \bold{63}, 113-122. (The ``cubic'' estimator.)
  
  Obenchain RL. (1978) Good and Optimal Ridge Estimators. \emph{Annals of Statistics}
  \bold{6}, 1111-1121. \doi{10.1214/aos/1176344314}
  
  Obenchain RL. (2021) \emph{The Efficient Shrinkage Path: Maximum Likelihood of Minimum MSE
  Risk.} \url{https://arxiv.org/abs/2103.05161}
  
  Obenchain RL. (2022) Efficient Generalized Ridge Regression. \emph{Open Statistics}
  \bold{3}: 1-18. \doi{10.1515/stat-2022-0108}

  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.3. \url{http://localcontrolstatistics.org}  
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{MLcalc}}, \code{\link{meff}}, \code{\link{correct.signs}}, \code{\link{MLtrue}} 
and \code{\link{RXpredict}}.}
\examples{
  data(longley2)
  form <- Employed~GNP+GNP.deflator+Unemployed+Armed.Forces+Population+Year
  rxefobj <- eff.ridge(form, data=longley2)
  rxefobj          # print shrinkage summary statistics...
  plot(rxefobj)    # 5 TRACEs on 1 plot...
  str(rxefobj)
}
\keyword{regression}
