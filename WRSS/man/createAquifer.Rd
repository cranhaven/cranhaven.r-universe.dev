\name{createAquifer}
\alias{createAquifer}
\title{
Constructor for class of \code{createAquifer}
}
\description{
this function constructs an object from class of \code{createAquifer} that prescribes a simplified lupmped model of unconfined aquifer.
}
\usage{
createAquifer(name, area, volume,
              rechargeTS, Sy, leakageFraction,
              initialStorage, leakageObject, priority)
}
\arguments{
  \item{name}{
(optional) A string: the name of the aquifer
}
  \item{area}{The area of aquifer (Km^2)}
  \item{volume}{The aquifer volume (MCM)}
  \item{rechargeTS}{(optional) A vector : a vector of water flowing into the aquifer (MCM)}
  \item{Sy}{Specific yield (default: 0.1)}
  \item{leakageFraction}{(optional) The leakage coeffcient of aquifer storage. The leakage is computed as the product of \code{leakageFraction} and aquifer storage. It is in \code{[0, 1]} interval}
  \item{initialStorage}{(optional) The initial volume of aquifer in the first step of the simulation (MCM). If missing, the function iterates to carry over the aquifer.}
  \item{leakageObject}{(optional) an object; from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}; which leakage volume pours to it.}
  \item{priority}{(optional) An integer: the supplying priority. \code{priority} is a value in [1, 99] interval. If missing, the \code{priority} is set to Inf.}
}
\value{
An object from class of \code{createAquifer}
}
\references{
Mart nez-Santos, P., and J. M. Andreu. "Lumped and distributed approaches to model natural recharge in semiarid karst aquifers." Journal of hydrology 388.3 (2010): 389-398.
}
\author{
Rezgar Arabzadeh
}
\seealso{\code{\link[WRSS]{addObjectToArea}}}
\keyword{ list }
