\name{eff.aug}
\alias{eff.aug}
\title{Augment calculations performed by eff.ridge() to prepare for display of eliptical confidence
 regions for pairs of biased coefficient estimates using plot.eff.biv()}
\description{
  This function makes classical (rather than Bayesian) Normal distribution-theory calculations of the
  form proposed in Obenchain(1977). Instead of providing "new" confidence regions for estimable linear 
  functions, Generalized Ridge Regression (GRR) can focus interest on estimates that are within
  traditional confidence intervals and regions but which deviate reasonably from the centroid of that
  interval or region.
}
\usage{
  eff.aug(efobj)
}
\arguments{
  \item{efobj}{An output object of class "eff.ridge".}
}
\value{ An output list object of class "eff.aug"...
  \item{p}{Number of regression predictor variables.}
  \item{LMobj}{The lm() output object for the model fitted using eff.ridge().}
  \item{bstar}{The p by 3 matrix of GRR coefficients. Column 1 contains OLS estimates, the middle
   column gives optimally biased coefficient estimates corresponding to the "Interior Knot" on all
   p of the Two-Piece Linear Splines, and column 3 contains all zeros for the Shrinkage Terminus.}
  \item{mcal}{Three increasing measures of shrinkage "Extent". The first is 0 for the OLS (BLUE)
   estimate, the second is the Maximum Likelihood m-Extent of Shrinkage [PURPLE point], and the
   third is m = p for Shrinkage to beta = 0. This "shrinkage terminus" [BLACK point] is frequently
   outside of the eff.biv() plot frame ...allowing the ellipse to be as LARGE as possible.}
  \item{vnams}{Names of all variables actually used in the GRR model.} 
}
\references{
  
  Obenchain RL. (1977) Classical F-tests and Confidence Regions for Ridge Regression.
  \emph{Technometrics} \bold{19}, 429-439. \doi{10.1080/00401706.1977.10489582}
  
  Obenchain RL. (2021) \emph{The Efficient Shrinkage Path: Maximum Likelihood of Minimum MSE
  Risk.} \url{https://arxiv.org/abs/2103.05161}
  
  Obenchain RL. (2022) Efficient Generalized Ridge Regression. \emph{Open Statistics}
  \bold{3}: 1-18. \doi{10.1515/stat-2022-0108}

  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.1. \url{http://localcontrolstatistics.org}  
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{eff.ridge}} and \code{\link{meff}}}
\keyword{regression}
