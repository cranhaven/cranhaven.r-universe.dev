\name{distr.func.help}
\alias{distr.func.help}
\alias{poly.part}

\title{These functions are used for calculating the integral of the
  B-spline density basis.}

\description{
These functions cooperate with each other for calculating the
integral of the B-spline density basis. functions. 'distr.func.help' is the main program, calling
'poly.part', calculating the integral of the B-spline density basis in sections between neighbouring knots. 'distr.func.help' calculates analytical functions of the integral. Therefore the function 'poly.part' is needed to construct these functions.
}

\usage{
distr.func.help(base,knots,penden.env,q,y,index)
poly.part(i,j,knots,help.env,q, yi=NULL, poly=FALSE)
}

\arguments{
  \item{base}{values of the considered B-spline basis}
  \item{knots}{knots of the considered B-spline basis}
  \item{penden.env}{Containing all information, environment of pencopula()}
  \item{q}{degree of the B-Spline basis}
  \item{y}{data of the marginal B-spline basis}
  \item{index}{columns of the whole B-spline basis, each hierarchy level
  is integrated separately}
  \item{i}{internal values for calculating the polynomials of each B-Spline}
  \item{j}{internal values for calculating the polynomials of each
    B-Spline}
  \item{help.env}{internal environment of pencopula() for calculating
    the integral}
  \item{yi}{internal values for calculating the polynomials of each
    B-Spline}
  \item{poly}{internal value, TRUE/FALSE}
}

\value{
\item{distr.func.help}{creating environment 'help.env', creating help points between each two neighbouring knots and calculates the integral each basis}
\item{poly.part}{using in 'distr.func.help' for creating the polynomial functions of each interval of each two neighbouring knots}
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}

\keyword{math}
