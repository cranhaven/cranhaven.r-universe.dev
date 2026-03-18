\name{knots.start}
\alias{knots.start}
\alias{knots.transform}
\alias{knots.order}
\title{Calculating the knots.}

\description{Calculating the equidistant knots for the estimation. Moreover,
  transformation of the knots are possible.}

\usage{
  knots.start(penden.env)
  knots.transform(d,alpha = 0, symmetric = TRUE)
  knots.order(penden.env)
}

\arguments{
  \item{penden.env}{Containing all information, environment of
    pencopula()}
  \item{d}{Hierarchy level of the marginal hierarchical B-spline basis.}
  \item{alpha}{Default = 0. Alpha is a tuning parameter, shifting the
    knots.}
  \item{symmetric}{Default = TRUE. If FALSE, the knots are selected
    without symmetry.}
}

\details{
  'Knots.order' sorts the knots in the order, in which they disappear
  in the hierarchical B-spline basis.
}

\value{
  \item{knots}{Selected and sorted marginal knots for the estimation.}
  \item{knots.help}{Extended set of knots. It is needed for calculating the distribution
    function, help points for the integration of the B-spline density basis.}
  \item{k.order}{Order of the knots, corresponding to their order in the
  hierarchical B-spline density basis.}
  \item{knots.t}{The knots ordered with 'k.order' for further fucntions.}
  \item{tilde.Psi.knots.d}{Hierarchical B-Spline density basis for 'knots'.}
  \item{tilde.Psi.knots.d.help}{Hierarchical B-Spline density basis for
    'knots.help'.}
  All values are saved in the environment.
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Flexible Copula Density Estimation with Penalized
  Hierarchical B-Splines, Kauermann G., Schellhase C. and Ruppert, D. (2013), Scandinavian Journal of Statistics 40(4), 685-705.

Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}
