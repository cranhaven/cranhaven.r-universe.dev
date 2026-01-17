\name{set.as}
\alias{set.as}
\title{
WRSS objects connector 
}
\description{
this function connects a \code{base} object as a either of: \code{'downstream'}, \code{'supplier'}, \code{'leakageObject'}, \code{'seepageObject'}, or \code{'divertObject'} to a \code{target} object, which are both instantiated by WRSS constructors.
}
\usage{
set.as(base,target,type='downstream')
}
\arguments{
  \item{base}{An object; from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}}
  \item{target}{An object; from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}}
  \item{type}{the type of \code{base} object to be set as to the \code{target} object: \code{'downstream'}, \code{'supplier'}, \code{'leakageObject'}, \code{'seepageObject'}, or \code{'divertObject'}}
}
\value{
an object from class of \code{target} object.
}
\author{
Rezgar Arabzadeh
}
\seealso{\code{\link[WRSS]{addObjectToArea}}}
\keyword{ list }
