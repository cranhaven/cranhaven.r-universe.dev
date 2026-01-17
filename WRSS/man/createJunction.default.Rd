\name{createJunction.default}
\alias{createJunction.default}
\title{
default function for class of \code{createJunction}
}
\description{
this function constructs an object from class of \code{createDiversion}, acting as a junction in the basin which is able to aggregate outflow water from upper tributaries and/or objects in the upstream.
}
\usage{
\method{createJunction}{default}(name = "junc1", downstream = NA)
}
\arguments{
  \item{name}{(optional) A string: the name of the junction}
  \item{downstream}{(optional) An object from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}};  which outflow volume pours to it.}
}
\value{
An object from class of \code{list}
}

\seealso{\code{\link[WRSS]{createJunction}}}
\keyword{list}
