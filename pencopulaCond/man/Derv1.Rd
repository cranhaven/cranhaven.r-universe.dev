\name{Derv1}
\alias{Derv1}
\title{Calculating the first derivative of the pencopula likelihood function w.r.t. parameter b}
\description{
 Calculating the first derivative of the pencopula likelihood function w.r.t. parameter b. 
}
\usage{
Derv1(penden.env,temp.lam=FALSE,temp.ck=FALSE)
}

\arguments{
  \item{penden.env}{Containing all information, environment of pencopula().}
  \item{temp.lam}{Calculating with temporal smoothing parameter lambda}
  \item{temp.ck}{Calculating with temporal weights ck of the spline basis functions}
}


\value{
  \item{Derv1.pen}{first order derivation of the penalized likelihood.}
  Derv1.pen is saved in the environment.
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Flexible Copula Density Estimation with Penalized
  Hierarchical B-Splines, Kauermann G., Schellhase C. and Ruppert, D. (2013), Scandinavian Journal of Statistics 40(4), 685-705.

Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}
\keyword{math}

