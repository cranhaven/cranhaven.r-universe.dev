\name{createRiver.base}
\alias{createRiver.base}
\title{
base function for class of \code{createRiver}
}
\description{
this function constructs an object from class of \code{createRiver}, which is able to act as a chanel or resource to supply a seort of demand(s).
}
\usage{
\method{createRiver}{base}(name, downstream, seepageFraction,
                seepageObject, discharge, priority)
}
\arguments{
  \item{name}{(optional) A string: the name of the river}
  \item{downstream}{(optional) An object; from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}; which outflow volume pours to it.}
  \item{seepageFraction}{(optional) The seepage coeffcient of river discharge flow. The seepage is computed as the product of \code{seepageFraction} and river discharge. It is in \code{[0, 1]} interval}
  \item{seepageObject}{(optional) An object; from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}; which seepage volume pours to it.}
  \item{discharge}{(optional) A vector: is a vector of river discharge time series (MCM).}
  \item{priority}{(optional) An integer: the supplying priority. \code{priority} is a value in [1, 99] interval. If missing, the \code{priority} is set to \code{Inf}.}
}
\value{
An object from class of \code{list}
}

\seealso{\code{\link[WRSS]{createRiver}}}

\keyword{ list }