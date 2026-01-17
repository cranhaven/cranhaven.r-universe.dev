\name{createDemandSite.base}
\alias{createDemandSite.base}
\title{
base function for class of \code{createDemandSite}
}
\description{
this function constructs an object from class of \code{createDemandSite}, which represents a demand site such as domestic, agricultural, and etc, with a specified demand time series.
}
\usage{
\method{createDemandSite}{base}(name, demandTS, demandParams, 
                   returnFlowFraction, suppliers,
                   downstream, priority)
}
\arguments{
  \item{name}{(optional) A string: the name of the demand site}
  \item{demandTS}{A vector: a vector of demand time series (MCM). If \code{demandParams} is null, providing the \code{demandTS} is compulsory.}
  \item{demandParams}{
A list: If \code{demandTS} is missing, the \code{demandParams} must be provided to establish \code{demandTS}. The \code{demandParams} includes three parts as follows:
   \itemize{
     \item \code{waterUseRate}: The total water demand per hectare (MCM) per a given water cycle
     \item \code{waterVariation}: A vector of the precentages for water demand distribution within a water cycle (the precentages in each \code{interval}). For instance, if the cycle is annually and the interval is \code{'month'}ly, the \code{waterVariation} could be a vector of length of 12, for which its indices signify the monthly portion of water demand, in precentage, by the total water demand required for the whole cycle
     \item \code{cropArea}: the area of cropping farms (in hectare)
   }
}
  \item{returnFlowFraction}{(optional) \code{returnFlowFraction} is fraction of total supplied water to the demand site. The return flow is computed as the product of \code{returnFlowFraction} and the amount of water the demand sites receives. \code{returnFlowFraction} must be in [0, 1] interval.}
  \item{suppliers}{(optional) A list of object(s) inherited from the folowing classes: \code{\link[WRSS]{createAquifer}}, \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, code{\link[WRSS]{createDiversion}}.}
  \item{downstream}{(optional) An object from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}; which return flow volume pours to it.}
  \item{priority}{(optional) An integer: the priority to be supplied. A value in [1, 99] interval.}
}
\value{
An object from class of \code{list}
}

\seealso{\code{\link[WRSS]{createDemandSite}}}

\keyword{ list }
