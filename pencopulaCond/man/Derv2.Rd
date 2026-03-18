\name{Derv2}
\alias{Derv2}
\title{Calculating the second order derivative with and without penalty.}
\description{Calculating the second order derivative with and without penalty.
}
\usage{
Derv2(penden.env, temp.lam = FALSE,temp.ck=FALSE,lam.fit=NULL)
}

\details{
  We approximate the second order derivative in this approach with the negative fisher information. 
}

\arguments{
  \item{penden.env}{Containing all information, environment of pendensity()}
  \item{temp.lam}{Calculating with temporal smoothing parameter lambda}
  \item{temp.ck}{Calculating with temporal weights ck of the spline basis functions}
  \item{lam.fit}{Indicating if the iterations for a new lambda are running}
}

\value{
  \item{Derv2.pen}{second order derivative w.r.t. beta with penalty}
  \item{Derv2.cal}{second order derivative w.r.t. beta without
    penalty. Needed for calculating of e.g. AIC.}
  Derv2.cal and Derv2.pen are saved in the environment.
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Flexible Copula Density Estimation with Penalized
  Hierarchical B-Splines, Kauermann G., Schellhase C. and Ruppert, D. (2013), Scandinavian Journal of Statistics 40(4), 685-705.

Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}

\keyword{math}

