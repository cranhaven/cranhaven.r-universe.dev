\name{createJunction}
\alias{createJunction}
\title{
Constructor for class of \code{createJunction}
}
\description{
this function constructs an object from class of \code{createDiversion}, acting as a junction in the basin which is able to aggregate outflow water from upper tributaries and/or objects in the upstream.
}
\usage{
createJunction(name, downstream)
}
\arguments{
  \item{name}{(optional) A string: the name of the junction}
  \item{downstream}{(optional) An object from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}};  which outflow volume pours to it.}
}
\value{
An object from class of \code{createJunction}
}
\author{
Rezgar Arabzadeh
}
\seealso{\code{\link[WRSS]{addObjectToArea}}}
\keyword{list}
