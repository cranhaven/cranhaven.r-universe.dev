\name{eff.biv}
\alias{eff.biv}
\title{Specify pairs of GRR Coefficient Estimates for display in Bivariate Confidence Regions}
\description{
  This function specifies which Pair of GRR estimates to display and the single (or dual) Confidence
  Level(s) of the Ellipse(s) displayed. Reguested confidence levels must both be equal to or greater
  than 0.05 and less than or equal to 0.95.
}
\usage{
  eff.biv(efaug, x1 = 1, x2 = 2, conf1 = 0.95, conf2 = 0.50) 
}
\arguments{
  \item{efaug}{An output list object of class "eff.aug" for a GRR model with p >= 2 x-Variables.}
  \item{x1}{Integer index value >= 1 and <= p for the x-Coefficient to be displayed on the horizontal axis.}
  \item{x2}{Integer index value >= 1 and <= p for the x-Coefficient to be displayed on the vertical axis.
   Index x2 must differ from x1 to display a plot.}
  \item{conf1}{This first Confidence level must be >= 0.05 and <= 0.95 to display an Ellipse.}
  \item{conf2}{When the second Confidence level is >= 0.05 and <= 0.95, its Ellipse is displayed. No
   plot is displayed when both conf1 and conf2 are outside of the [0.05, 0.95] range.}   
}
\value{ An output list object of class "eff.biv"...
  \item{p}{Number of regression predictor variables.}
  \item{LMobj}{The lm() output object for the model fitted using eff.ridge().}
  \item{bstar}{The p by 3 matrix of shrunken GRR coefficients. The 3 columns correspond to OLS
   estimates, optimally shrunken estimates, and estimates shrunken to Zeros.}
  \item{mcal}{Three increasing measures of shrinkage m-Extent: 0 for OLS [BLUE], the Optimal m-Extent
   at the "Interior Knot" [PURPLE], and m = p [BLACK] at the Shrinkage Terminus.}
  \item{ellip1}{matrix[100, 2] of points on confidence ellipse 1.}
  \item{conf1}{confidence level of ellipse 1 within [0.05, 0.95].}
  \item{ecor1}{Pearson correlation between x1 and x2 coordinates.}
  \item{ellip2}{matrix[100, 2] of points on confidence ellipse 2.}
  \item{conf2}{confidence level of ellipse 2 within [0.05, 0.95].}
  \item{ecor2}{Pearson correlation between x1 and x2 coordinates.}
}
\references{
  
  Obenchain RL. (1977) Classical F-tests and Confidence Regions for Ridge Regression.
  \emph{Technometrics} \bold{19}, 429-439. \doi{10.1080/00401706.1977.10489582}
  
  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.1. \url{http://localcontrolstatistics.org}
  
  Murdoch DJ. and Chow ED. (1996). A graphical display of large correlation matrices.
  \emph{The American Statistician} 50, 178-180.
  
  Murdoch DJ. ellipse: Functions for Drawing Ellipses and Ellipse-Like Confidence Regions.
  \url{https://CRAN.R-project.org/package=ellipse}
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{eff.aug}}, \code{\link{ellipse.lm}}, \code{\link{eff.ridge}}}
\keyword{regression}