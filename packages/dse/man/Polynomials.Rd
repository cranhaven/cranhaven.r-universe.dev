\name{Polynomials}
\alias{characteristicPoly}
\alias{companionMatrix}
\alias{polyvalue}
\alias{polydet}
\alias{polyprod}
\alias{polysum}
\alias{polyrootDet}
        
\title{Polynomial Utilities}
\description{Polynomial utility functions used by DSE.}
\usage{
    characteristicPoly(a)
    companionMatrix(a)
    polyvalue(coef, z)
    polydet(a)
    polyprod(a, b)
    polysum(a, b)
    polyrootDet(a)
}
\arguments{
    \item{a}{An array representing a matrix polynomial.}
    \item{b}{An array representing a matrix polynomial.}
    \item{coef}{Coefficients of a polynomial.}
    \item{z}{Value at which the polynomial is to be evaluated.}
}
\value{depends}
\details{
    These are utility functions used in some ARMA model calculations
    such as root and stability calculations.}
\seealso{
    \code{\link{polyroot}}
    \code{\link{roots}}
    \code{\link{stability}}
}

\concept{DSE}
\keyword{ts}

