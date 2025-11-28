\name{uc.lars}
\alias{uc.lars}
\title{Maximum Likelihood Least Angle Regression on Uncorrelated X-Components}
\description{Apply least angle regression estimation to the uncorrelated
  components of a possibly ill-conditioned linear regression model and
  generate normal-theory maximum likelihood TRACE displays.
}
\usage{
  uc.lars(form, data, rscale = 1, type = "lar", trace = FALSE, 
    eps = .Machine$double.eps, omdmin = 9.9e-13) 
}
\arguments{
  \item{form}{A regression formula [y~x1+x2+...] suitable for use with lm().}
  \item{data}{Data frame containing observations on all variables in the formula.}
  \item{rscale}{One of three possible choices (0, 1 or 2) for "rescaling" of variables
   (after being "centered") to remove all "non-essential" ill-conditioning: 0 implies no
   rescaling; 1 implies divide each variable by its standard error; 2 implies rescale as
   in option 1 but re-express answers as in option 0.}
  \item{type}{One of "lasso", "lar" or "forward.stagewise" for function lars(). Names can be
   abbreviated to any unique substring. Default in uc.lars() is "lar".}
  \item{trace}{If TRUE, lars() function prints out its progress.}
  \item{eps}{The effective zero for lars().}
  \item{omdmin}{Strictly positive minimum allowed value for one-minus-delta (default = 9.9e-013.)}
}
\details{uc.lars() applies Least Angle Regression to the uncorrelated components of a
 possibly ill-conditioned set of x-variables.  A closed-form expression for the lars/lasso
 shrinkage delta factors exits in this case: Delta(i) = max(0,1-k/abs[PC(i)]), where PC(i)
 is the principal correlation between y and the i-th principal coordinates of X. Note that
 the k-factor in this formulation is limited to a subset of [0,1].  MCAL=0 occurs at k=0,
 while MCAL = p results when k is the maximum absolute principal correlation. 
}
\value{ An output list object of class uc.lars:
  \item{form}{The regression formula specified as the first argument.}
  \item{data}{Name of the data.frame object specified as the second argument.}
  \item{p}{Number of regression predictor variables.}
  \item{n}{Number of complete observations after removal of all missing values.}
  \item{r2}{Numerical value of R-square goodness-of-fit statistic.}
  \item{s2}{Numerical value of the residual mean square estimate of error.}
  \item{prinstat}{Listing of principal statistics.}
  \item{gmat}{Orthogonal matrix of direction cosines for regressor principal axes.}
  \item{lars}{An object of class lars.}
  \item{coef}{Matrix of shrinkage-ridge regression coefficient estimates.}
  \item{risk}{Matrix of MSE risk estimates for fitted coefficients.}
  \item{exev}{Matrix of excess MSE eigenvalues (ordinary least squares minus ridge.)}
  \item{infd}{Matrix of direction cosines for the estimated inferior direction, if any.}
  \item{spat}{Matrix of shrinkage pattern multiplicative delta factors.}
  \item{mlik}{Listing of criteria for maximum likelihood selection of M-extent-of-shrinkage.}
  \item{sext}{Listing of summary statistics for all M-extents-of-shrinkage.}
  \item{mClk}{Most Likely Extent of Shrinkage Observed: best multiple of (1/steps) <= p.} 
  \item{minC}{Minimum Observed Value of Normal-theory -2*log(Likelihood).}
}
\references{

  Hastie T, Efron, B. (2013) lars: Least Angle Regression, Lasso and Forward Stagewise.
  ver 1.2, \url{https://CRAN.R-project.org/package=lars}

  Obenchain RL. (1994-2005) Shrinkage Regression: ridge, BLUP, Bayes, spline and Stein.
  \url{http://localcontrolstatistics.org}
  
  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.1. \url{http://localcontrolstatistics.org}
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{aug.lars}}.}
\examples{
  data(longley2)
  form <- GNP~GNP.deflator+Unemployed+Armed.Forces+Population+Year+Employed
  rxucobj <- uc.lars(form, data=longley2)
  rxucobj
  plot(rxucobj)
  str(rxucobj)
}
\keyword{regression}
