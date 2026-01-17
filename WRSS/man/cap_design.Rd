\name{cap_design}
\alias{cap_design}
\title{
  Constructor for class of \code{cap_design}
}
\description{
  Calculates the RRV measures for multiple design candidates.
}
\usage{
  cap_design(area,params,w,plot)
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
\examples{
  Res1<-createReservoir(name="res1",type='storage',
                        priority=1,netEvaporation=rnorm(120,0.5,0.1),
                        geometry=list(deadStorage= 10 ,capacity= 50 ,
                                      storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
  R1<-createRiver(name="river1",discharge=rnorm(120,25,1.5))
  waterVariation<-round(sin(seq(0,pi,length.out=12))*
                          100/sum(sin(seq(0,pi,length.out=12))))
  D1<-createDemandSite(name ="Agri1",
                       demandParams=list(waterUseRate=1,
                                         waterVariation=waterVariation,
                                         cropArea=500),
                       returnFlowFraction =0.2,priority=2)
  area<-createArea(name="unknown",location="unknown",
                   simulation=list(start='2000-01-01',
                                   end  ='2000-04-29',
                                   interval='day'))
  R1<-set.as(Res1,R1,'downstream')
  D1<-set.as(Res1,D1,'supplier')
  area<-addObjectToArea(area,R1)
  area<-addObjectToArea(area,Res1)
  area<-addObjectToArea(area,D1)
  params<-list(
    list(Res1,seq(0.5,1.5,0.1))
  )
  cap_design(area,params)
}

\seealso{\code{\link[WRSS]{addObjectToArea}}}
\keyword{ list }