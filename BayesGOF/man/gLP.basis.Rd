\name{gLP.basis}
\alias{gLP.basis}
\title{
Determine LP basis functions for prior distribution \eqn{g}
}
\description{Determines the LP basis for a given parametric prior distribution.
}
\usage{
gLP.basis(x, g.par, m, con.prior, ind)
}
\arguments{
  \item{x}{ \code{x} values (integer or vector) from 0 to 1.}
  \item{g.par}{ Estimated parameters for specified prior distribution (i.e beta prior: \eqn{\alpha} and \eqn{\beta}; normal prior: \eqn{\mu} and \eqn{\tau^2}; gamma prior: \eqn{\alpha} and \eqn{\beta}).}
  \item{m}{Number of LP-Polynomial basis.}
  \item{con.prior}{Specified conjugate prior distribution for basis functions.  Options are \code{"Beta"}, \code{"Normal"}, and \code{"Gamma"}.}
  \item{ind}{Default is NULL which returns matrix with \eqn{m} columns that consists of LP-basis functions; user can provide a specific choice through \code{ind}.}
}
\value{ Matrix with \code{m} columns of values for the LP-Basis functions evaluated at \code{x}-values.}
\references{
Mukhopadhyay, S. and Fletcher, D., 2018. "Generalized Empirical Bayes via Frequentist Goodness of Fit," \emph{Nature Scientific Reports}, 8(1), p.9983, \url{https://www.nature.com/articles/s41598-018-28130-5 }.

Mukhopadhyay, S., 2017. "Large-Scale Mode Identification and Data-Driven Sciences," \emph{Electronic Journal of Statistics, 11}(1), pp.215-240.

Mukhopadhyay, S. and Parzen, E., 2014. "LP Approach to Statistical Modeling," arXiv: 1405.2601.

}
\author{  Subhadeep Mukhopadhyay, Doug Fletcher}
\keyword{DS Main Functions}