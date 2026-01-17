\name{createDiversion.base}
\alias{createDiversion.base}
\title{
base function for class of \code{createDiversion}
}
\description{
this function constructs an object from class of \code{createDiversion}, acting as a diversion dam which is able to divert water up to a specified capacity.
}
\usage{
\method{createDiversion}{base}(name, capacity,
               divertObject, downstream, priority)
}
\arguments{
  \item{name}{(optional) A string: the name of the diversion}
  \item{capacity}{The maximum capacity of diversion dam (CMS).}
  \item{divertObject}{(optional) An object from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}; which recieves the diverted water volume.}
  \item{downstream}{(optional) An object from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}; which overflow volume pours to it.}
  \item{priority}{(optional) An integer: the supplying priority. \code{priority} is a value in [1, 99] interval. If missing, the \code{priority} is set to \code{Inf}.}
}
\value{
An object from class of \code{list}
}

\seealso{\code{\link[WRSS]{createDiversion}}}
\keyword{list}
