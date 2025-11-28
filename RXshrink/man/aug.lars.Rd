\name{aug.lars}
\alias{aug.lars}
\title{Maximum Likelihood Estimation of Effects in Least Angle Regression}
\description{
  These functions perform calculations that determine whether least angle and lasso regression
  estimates correspond to generalized ridge regression (GRR) estimates (i.e. whether they use
  shrinkage delta-factors that are both non-negative and strictly less than 1.0). They also
  estimate the Normal-theory likelihood that MSE risk is minimized and compute diagnostics for
  display in ridge TRACE plots.
}
\usage{
aug.lars(form, data, rscale = 1, type = "lar", trace = FALSE, 
    eps = .Machine$double.eps, omdmin = 9.9e-13) 
}
\arguments{
  \item{form}{A regression formula [y~x1+x2+...] suitable for use with lm().}
  \item{data}{Data frame containing observations on all variables in the formula.}
  \item{rscale}{One of three possible choices (0, 1 or 2) for "rescaling" of variables
   (after being "centered") to remove all "non-essential" ill-conditioning:  0 implies no
   rescaling; 1 implies divide each variable by its standard error; 2 implies rescale as
   in option 1 but re-express answers as in option 0.}
  \item{type}{One of "lasso", "lar" or "forward.stagewise" for function lars(). Names can be
   abbreviated to any unique substring. Default in aug.lars() is "lar".}
  \item{trace}{If TRUE, lars() function prints out its progress.}
  \item{eps}{The effective zero for lars().}
  \item{omdmin}{Strictly positive minimum allowed value for one-minus-delta (default = 9.9e-013.)}
}
\details{aug.lars() calls the Efron/Hastie lars() function to perform Least Angle Regression on
 x-variables that have been centered and possibly rescaled but which may be (highly) correlated.
 Maximum likelihood TRACE displays paralleling those of eff.ridge() and qm.ridge() are also
 computed and (optionally) plotted. 
}
\value{ An output list object of class aug.lars:
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
  \item{minC}{Minimum Observed Value of Normal-theory -2*log(Likelihood-Ratio).}    
}
\references{
  Breiman L. (1995) Better subset regression using the non-negative garrote.
  \emph{Technometrics} \bold{37}, 373-384.

  Efron B, Hastie T, Johnstone I, Tibshirani R. (2003)
  Least angle regression. \emph{Annals of Statistics} \bold{32}, 407-499.
  
  Hastie T, Efron, B. (2013) lars: Least Angle Regression, Lasso and Forward Stagewise.
  ver 1.2, \url{https://CRAN.R-project.org/package=lars}  
  
  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.1. \url{http://localcontrolstatistics.org}

  Tibshirani R. (1996) Regression shrinkage and selection via the lasso.
  \emph{J. Roy. Stat. Soc. B}  \bold{58}, 267-288.
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{uc.lars}}.}
\examples{
  data(longley2)
  form <- GNP~GNP.deflator+Unemployed+Armed.Forces+Population+Year+Employed
  rxlobj <- aug.lars(form, data=longley2)
  rxlobj
  plot(rxlobj)
  str(rxlobj)
}
\keyword{regression}
