\name{meff}
\alias{meff}
\title{m-Extents of Shrinkage used in eff.ridge() Calculations.}
\description{
  The meff() function computes the numerical Shrinkage delta-factors corresponding to any
  desired m-Extent of Shrinkage, meobj, along the "efficient" (shortest) Path used by
  eff.ridge(). This Two-Piece Linear Path has its single "interior" Knot at the
  Normal-theory Maximum Likelihood estimate with Minimum MSE Risk: d[j] = dMSE[j] for
  j = 1, 2, ..., p.   
}
\usage{
  meff(meobj, p, dMSE)
}
\arguments{
  \item{meobj}{A desired m-Extent of Shrinkage along the "efficient" Path.}
  \item{p}{The integer number of non-constant x-variables used in defining the linear
   model being fitted to ill-conditioned (intercorrelated, confounded) data. Note that
   p must also be rank of the given X-matrix.}
  \item{dMSE}{Maximum Likelihood [ML] estimates of Shrinkage Delta-Factors leading to
   minimum MSE risk.}
}
\value{The appropriate scalar value for m and corresponding p by p diagonal matrix d:
  \item{meobj}{Any desired m-Extent of Shrinkage (a scalar) within [0, p].}
  \item{d}{The p by p diagonal matrix of shrinkage-factors: d[j,j] in [0, 1].}
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{eff.ridge}} and \code{\link{eff.aug}}.}
\keyword{regression}
