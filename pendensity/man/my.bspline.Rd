\name{my.bspline}
\alias{my.bspline}
\title{my.bspline}
\description{
Integrates the normal B-Spline Base to a value of one. The dimension of the base depends on the input of number of knots 'k' and of the order of the B-Spline base 'q'.
}
\usage{
my.bspline(h, q, knots.val, y, K, plot.bsp)
}

\arguments{
  \item{h}{if equidistant knots are just (default in pendensity()), h is the distance between two neighbouring knots}
  \item{q}{selected order of the B-Spline base}
  \item{knots.val}{selected values for the knots}
  \item{y}{values of the response variable y}
  \item{K}{the number of knots K for the construction of the base}
  \item{plot.bsp}{Indicator variable TRUE/FALSE if the integrated B-Spline base should be plotted}
}

\details{
Firstly, the function constructs the B-Spline base to the given number of knots 'K' and the given locations of the knots 'knots.val\$val. Due to the recursive construction of the B-Spline, for all orders greater than 2, the dimension of the B-Spline base of given K grows up with help.degree=q-2. Avoiding open B-Splines at the boundary, we simulate 6 extra knots at both ends of the support, saved in knots.val\$all. Therefore, we get normal B-Splines at the given knots 'knots.val\$val'. For these knots, we construct the B-Spline base of order 'q' and for order 'q+1' (using for calculation the distribution). Additionally, we save q-1 knots at both ends of the support of 'knots.val\$val'. After construction, we get a base of dimension K=K+help.degree. So, we define our value K and cut our B-Spline base at both ends to get the adequate base due to the order 'q' and the number of knots 'K'. For the base of order 'q+1', we need to get an additional base, due to the construction of the B-Splines. Due to the fact, that we use equidistant knots, we can integrate our B-Splines very simple to the value of 1. The integration is done by the well-known factor q/(knots.val\$help[i+q]-knots.val\$help[i]). This results the standardization coefficients 'stand.num' for each B-spline (which are identically for equidistant knots). Moreover, one can draw the integrated base and, if one calls this function with the argument 'plot.bsp=TRUE'.
}
\value{
  \item{base.den}{The integrated B-Spline base of order q}
  \item{base.den2}{The integrated B-Spline base of order q+1}
  \item{stand.num}{The coefficients for standardization of the ordinary B-Spline base}
  \item{knots.val}{This return is a list. It consider of the used knots 'knots.val\$val', the help knots 'knots.val\$help' and the additional knots 'knots.val\$all', used for the construction of the base and the calculation of the distribution function of each B-Spline.}
  \item{K}{The transformed value of K, due to used order 'q' and the input of 'K'}
  \item{help.degree}{Due to the recursive construction of the B-Spline, for all orders greater than 2, the dimension of the B-Spline base of given K grows up with 'help.degree=q-2'. This value is returned for later use.}
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\note{
This functions uses the fda-package for the construction of the B-Spline Base.
}


\keyword{math}

