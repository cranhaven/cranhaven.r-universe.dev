\name{GOF.default}
\alias{GOF.default}
\title{
default function for class of \code{GOF}
}
\description{
this function calculates the goodness of fit (gof) using chi-squared test.
}
\usage{
\method{GOF}{default}(basin,object,observed)
}
\arguments{
  \item{basin}{An object from class of \code{sim}.}
  \item{object}{An object from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}; which is associated with \code{observed} time series and exists in the \code{basin}.}
  \item{observed}{ A vector of observed time series.}
}
\value{
A list with class \code{"htest"}.
}
\author{Rezgar Arabzadeh}

\seealso{\code{\link[WRSS]{GOF}}}

\keyword{ list }