\name{cap_design.base}
\alias{cap_design.base}
\title{
base function for class of \code{cap_design}
}
\description{
Calculates the RRV measures for multiple design candidates.
}
\usage{
\method{cap_design}{base}(area,params,w,plot)
}
\arguments{
  \item{area}{An object from class of \code{'createArea'}}
  \item{params}{A list of list(s), which each sub-list can contains an object from either of classes \code{'createDemandSite'} or \code{'createReservoir'} and a vector of scale factors multiplied to the set design parameters. For reservoirs the scale factor will be multiplied to the capacity for the and for demand site, it will be multiplied to the demand time series}
  \item{w}{(optional) A vector of weights of sustainability indices summing 1 with length of equal with the number of demand site objects built-in \code{'params'} argument or equal with number of demand sites supplied by the reservoirs built-in \code{'params'}. If missing the weights will be assumed equall}
  \item{plot}{(optional) logical: plot the resault or not. The default is \code{TRUE}}
}
\value{
A matrix of RRV and sustainability index proposed by Hashemitto et al. (1982) and Loucks (1997).
}
\references{
Hashimoto, T., Stedinger, J. R., & Loucks, D. P. (1982). Reliability, resiliency, and vulnerability criteria for water resource system performance evaluation. Water resources research, 18(1), 14-20.
Loucks, D. P. (1997). Quantifying trends in system sustainability. Hydrological Sciences Journal, 42(4), 513-530.
}
\author{
Rezgar Arabzadeh
}
\seealso{\code{\link[WRSS]{cap_design}}}
\keyword{ list }