\name{distr.func}
\alias{distr.func}
\alias{distr.func.help}
\alias{cal.int}
\alias{poly.part}

\title{These functions are used for calculating the empirical and theoretical distribution functions.}
\description{
These functions cooperate with each other for calculating the distribution functions. 'distr.func' is the main program, calling 'distr.func.help',generating an environment with needed values for calculating the distribution of each interval between two neighbouring knots. 'distr.func' returns analytical functions of the distribution of each interval between two neighbouring knots. Therefore the function 'poly.part' is needed to construct these functions. 'cal.int' evaluates these integrals, considering if the whole interval should be evaluated or if any discrete value 'yi' is of interest.
}

\usage{
distr.func(yi = NULL, obj, help.env=distr.func.help(obj))
distr.func.help(obj)
cal.int(len.b, q, help.env, knots.val)
poly.part(i,j,knots.val,help.env,q, yi=NULL, poly=FALSE)
}

\arguments{
  \item{yi}{if the distribution at any discrete point is of interest, you can call for it. Default=NULL doesn't consider any discrete point }
  \item{obj}{a object of class pendensity}
  \item{help.env}{object is generated with calling distr.func.help(obj)}
  \item{len.b}{length of B-Spline}
  \item{q}{order of the B-Spline}
  \item{knots.val}{values of the used knots}
  \item{poly}{TRUE/FALSE}
  \item{i}{internal values for calculating the polynomials of each B-Spline}
  \item{j}{internal values for calculating the polynomials of each B-Spline}
}

\value{
\item{distr.func}{returns analytical functions of the distributions between each two neighbouring intervals}
\item{distr.func.help}{creating environment 'help.env', creating help points between each two neighbouring knots and calculates the polynomial-coefficients of each base part}
\item{cal.int}{evaluating the result of distr.func. Thereby it's possible to call for an explicit distribution values F(yi)}
\item{poly.part}{using in 'distr.func' for creating the polynomial functions of each interval of each two neighbouring knots}
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}

\keyword{math}
