\name{MLcalc}
\alias{MLcalc}
\title{Calculate Efficient Maximum Likelihood (ML) point-estimates for a Linear Model that
  are either Unbiased (OLS) or Most Likely to be Optimally Biased under Normal-distribution
  theory.}
\description{
  Compute MSE risk-optimal point-estimates of Beta-Coefficients and their Relative MSE risks.
  Much of the code for this function is identical to that of eff.ridge(), which computes
  multiple points along the "Efficient" Shrinkage Path. MLcalc() restricts attention to
  only two points: [1] the Unbiased OLS (BLUE) vector and [2] the Most Likely to be Optimally
  Biased [Minimum MSE Risk] vector of estimates.}
\usage{
  MLcalc(form, data, rscale = 1)}
\arguments{
  \item{form}{A regression formula [y~x1+x2+...] suitable for use with lm().}
  \item{data}{Data frame containing observations on all variables in the formula.}
  \item{rscale}{One of three possible choices (0, 1 or 2) for "rescaling" of variables
   (after being "centered") to remove all "non-essential" ill-conditioning:  0 implies no
   rescaling; 1 implies divide each variable by its standard error; 2 implies rescale as
   in option 1 but re-express answers as in option 0.}
}
\details{Ill-conditioned and/or nearly multi-collinear regression models are unlikely to
  produce Ordinary Least Squares (OLS) regression coefficient estimates that are very
  close, numerically, to their unknown true values.  Specifically, OLS estimates can then
  tend to have "wrong" numerical signs and/or unreasonable relative magnitudes, while
  shrunken (generalized ridge) estimates chosen to Maximize their Likelihood of reducing
  Mean Squared Error (MSE) Risk (expected squared-error loss) can be more stable
  numerically.  On the other hand, because only OLS estimates are guaranteed to be minimax
  when risk is Matrix Valued (truly multivariate), no guarantee of an expected reduction in
  MSE Risk is necessarily associated with "Optimal" Generalized Ridge Regression shrinkage. 
}
\value{ An output list object of class MLcalc:
  \item{data}{Name of the data.frame object specified as the second argument.}
  \item{form}{The regression formula specified as the first argument.}
  \item{p}{Number of regression predictor variables.}
  \item{n}{Number of complete observations after removal of all missing values.}
  \item{r2}{Numerical value of R-square goodness-of-fit statistic.}
  \item{s2}{Numerical value of the residual mean square estimate of error.}
  \item{prinstat}{Listing of principal statistics.}
  \item{gmat}{Orthogonal Matrix of Direction Cosines for Principal Axes [1:p, 1:p].}
  \item{beta}{Numerical shrinkage-ridge regression coefficient estimates [1:2, 1:p].}
  \item{rmse}{Numerical MSE risk estimates for fitted coefficients [1:2, 1:p].}
  \item{dMSE}{Numerical delta-factors for shrinking OLS components [1:p].}
  \item{ys}{Numerical rescaling factor for y-outcome variable [1, 1].}
  \item{xs}{Numerical rescaling factors for given x-variables [1:p].}  
}
\references{

  Thompson JR. (1968) Some shrinkage techniques for estimating the mean. \emph{Journal
  of the American Statistical Association} \bold{63}, 113-122. (The ``cubic'' estimator.)
  
  Obenchain RL. (2021) \emph{The Efficient Shrinkage Path: Maximum Likelihood of Minimum MSE
  Risk.} \url{https://arxiv.org/abs/2103.05161}
  
  Obenchain RL. (2022) Efficient Generalized Ridge Regression. \emph{Open Statistics}
  \bold{3}: 1-18. \doi{10.1515/stat-2022-0108}
  
  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.1. \url{http://localcontrolstatistics.org}
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{eff.ridge}}, \code{\link{MLboot}}, \code{\link{eff.aug}}}
\keyword{regression}
