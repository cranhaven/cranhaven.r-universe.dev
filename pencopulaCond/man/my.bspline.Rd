\name{my.bspline}
\alias{my.bspline}
\alias{int.my.bspline}
\title{my.bspline}

\description{
'my.bspline' Integrates the normal B-Spline basis to a B-spline density
basis. The dimension of the basis depends on the input of number of
knots 'k' and of the order of the B-spline basis 'q'. 'int.my.bspline'
is a function for transformation of open B-spline basis at the
boundary to become a B-spline basis density.
}
\usage{
my.bspline(h, q, knots, y, K, plot.bsp, typ)
int.my.bspline(help.env)
}

\arguments{
  \item{h}{if equidistant knots are used (default in pencopula()), h is the distance between two neighbouring knots}
  \item{q}{selected order of the B-spline basis}
  \item{knots}{selected values for the knots}
  \item{y}{values of the response variable}
  \item{K}{the number of knots for the construction of the base}
  \item{plot.bsp}{Indicator variable TRUE/FALSE if the integrated
    B-spline basis should be plotted}
  \item{typ}{typ==1 without open B-splines at the boundary
    typ==2 with open B-splines at the boundary}
  \item{help.env}{Internal environment of my.bspline().}
}

\details{
  Firstly, the function constructs the B-spline basis to the given number
  of knots 'K' and the given locations of the knots.
}
\value{
  \item{base.den}{The integrated B-Spline base of order q}
  \item{stand.num}{The coefficients for standardization of the ordinary B-Spline basis}
  \item{knots.val}{This return is a list. It consider of the used knots 'knots.val\$val', the help knots 'knots.val\$help' and the additional knots 'knots.val\$all', used for the construction of the base and the calculation of the distribution function of each B-Spline.}
  \item{K}{The transformed value of K, due to used order 'q' and the input of 'K'}
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}

\keyword{math}

\references{Flexible Copula Density Estimation with Penalized
  Hierarchical B-Splines, Kauermann G., Schellhase C. and Ruppert, D. (2013), Scandinavian Journal of Statistics 40(4), 685-705.

Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}
