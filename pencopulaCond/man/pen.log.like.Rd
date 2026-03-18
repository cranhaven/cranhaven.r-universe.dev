\name{pen.log.like}
\alias{pen.log.like}
\title{Calculating the log likelihood}

\description{
Calculating the considered log likelihood.
}

\usage{
pen.log.like(penden.env,cal=FALSE,temp.lam=FALSE,temp.ck=FALSE)
}

\arguments{
  \item{penden.env}{Containing all information, environment of pencopula()}
  \item{cal}{if TRUE, the final weights of one iteration are used for
    the calculation of the penalized log likelihood.}
  \item{temp.lam}{Calculating with temporal smoothing parameter lambda}
  \item{temp.ck}{Calculating with temporal weights ck of the spline basis functions}
}
\details{
The calculation depends on the estimated weights b, the penalized
hierarchical B-splines Phi and the penalty paramters lambda.
}

\value{
  \item{pen.log.like}{Penalized log likelihood of the copula density.}
  \item{log.like}{Log-Likelihood of the copula density.}
The values are saved in the environment.
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}

\keyword{nonparametric}

\references{Flexible Copula Density Estimation with Penalized
  Hierarchical B-Splines, Kauermann G., Schellhase C. and Ruppert, D. (2013), Scandinavian Journal of Statistics 40(4), 685-705.

Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}
