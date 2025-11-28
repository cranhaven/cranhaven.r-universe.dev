\name{syxi}
\alias{syxi}
\title{Linear and GAM Spline Predictions from a Single x-Variable}
\description{
  Compute and display (x,y) plots with their linear and gam() spline y-predictions.
}
\usage{
  syxi(form, data, i = 1)
}
\arguments{
  \item{form}{A "simple" regression formula [y~x] suitable for use with lm().}
  \item{data}{data.frame containing at least 10 observations on both variables in the formula.}
  \item{i}{A single integer "index" within 1:25.}
}
\details{The gam() functon from the \emph{mgcv} R-package is used to compute and,
  subsequently, to generate plots that visually compare the "linear" fit from lm(y~x)
  with a potentially "nonlinear" fit using smoothing parameters. The horizontal axis on
  type = "sy" plots gives potentially "straightened out" x numerical values.
}
\value{ An output list object of class syxi:
  \item{dfname}{Name of the data.frame object specified as the second argument.} 
  \item{xname}{"xi" as Two or Three Characters.} 
  \item{sxname}{"si" as Two or Three Characters.}
  \item{dfsxf}{A data.frame containing 3 variables: "yvec", "xvec", and "sxfit".}    
  \item{yxcor}{Pearson correlation between "yvec" and "xvec".}
  \item{yscor}{Pearson correlation between "yvec" and "sxfit".}
  \item{xscor}{Pearson correlation between "xvec" and "sxfit".}
  \item{lmyxc}{lm() Coefficients (intercept and slope) for y ~ x.}
  \item{lmysc}{lm() Coefficients (intercept and slope) for y ~ sxfit.}
  \item{adjR2}{Adjusted R2 value from gam.sum$r.sq.}		
}
\references{
  Obenchain RL. (2022) Efficient Generalized Ridge Regression. \emph{Open Statistics}
  \bold{3}: 1-18. \doi{10.1515/stat-2022-0108}

  Obenchain RL. (2023) Nonlinear Generalized Ridge Regression. \emph{arXiv preprint}
  \url{https://arxiv.org/abs/2103.05161}  
}
\author{Bob Obenchain <wizbob@att.net>}
\examples{
  library(mgcv)
  data(longley2)
  form = GNP ~ Year
  GNPpred = syxi(form, data=longley2, i = 1)
  plot(GNPpred, type="xy")
  title(main="y = GNP on x1 = Year")
  plot(GNPpred, type="sy")
  title(main="y = GNP on Spline for Year")
}
\keyword{regression}
