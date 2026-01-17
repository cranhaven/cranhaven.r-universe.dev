\name{GOF}
\alias{GOF}
\title{
Goodness of fit
}
\description{
this function calculates the goodness of fit (gof) using chi-squared test.
}
\usage{
GOF(basin,object,observed)
}
\arguments{
  \item{basin}{An object from class of \code{sim}.}
  \item{object}{An object from either of classes of \code{\link[WRSS]{createAquifer}} , \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, \code{\link[WRSS]{createJunction}}, \code{\link[WRSS]{createDiversion}}, or \code{\link[WRSS]{createDemandSite}}; which is associated with \code{observed} time series and exists in the \code{basin}.}
  \item{observed}{ A vector of observed time series.}
}
\value{
A list with class \code{"htest"}.
}
\author{
Rezgar Arabzadeh}

\examples{
  J1<-createJunction(name="j1")
  Res1<-createReservoir(name="res1",type='storage',
                        priority=1,netEvaporation=rnorm(120,0.5,0.1),
                        geometry=list(deadStorage= 10 ,capacity= 90 ,
                                      storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
  Res2<-createReservoir(name="res2",type='storage',
                        priority=2,netEvaporation=rnorm(120,0.5,0.1),
                        geometry=list(deadStorage= 10 ,capacity= 90 ,
                                      storageAreaTable= cbind(seq(0,90,10),seq(0,9,1))))
  R1<-createRiver(name="river1",discharge=rnorm(120,5,1.5))
  R2<-createRiver(name="river2",discharge=rnorm(120,5,1.5))
  waterVariation<-round(sin(seq(0,pi,length.out=12))*
                          100/sum(sin(seq(0,pi,length.out=12))))
  D1<-createDemandSite(name ="Agri1",
                       demandParams=list(waterUseRate=1,
                                         waterVariation=waterVariation,
                                         cropArea=1000),
                       returnFlowFraction =0.2,priority=1)
  D2<-createDemandSite(name ="Agri2",
                       demandParams=list(waterUseRate=1,
                                         waterVariation=waterVariation,
                                         cropArea=1000),
                       returnFlowFraction =0.2,priority=2)
  D3<-createDemandSite(name ="Agri3",
                       demandParams=list(waterUseRate=1,
                                         waterVariation=waterVariation,
                                         cropArea=1000),
                       returnFlowFraction =0.2,priority=1)
  area<-createArea(name="unknown",location="unknown",
                   simulation=list(start='2000-01-01',
                                   end  ='2000-04-29',
                                   interval='day'))
  
  R1<-set.as(Res1,R1,'downstream')
  R2<-set.as(Res2,R2,'downstream')
  Res1<-set.as(J1,Res1,'downstream')
  Res2<-set.as(J1,Res2,'downstream')
  D1<-set.as(J1,D1,'downstream')
  D2<-set.as(J1,D2,'downstream')
  D3<-set.as(J1,D3,'downstream')
  D1<-set.as(Res1,D1,'supplier')
  D2<-set.as(Res1,D2,'supplier')
  D2<-set.as(Res2,D2,'supplier')
  D3<-set.as(Res2,D3,'supplier')
  
  area<-addObjectToArea(area,R1)
  area<-addObjectToArea(area,R2)
  area<-addObjectToArea(area,Res1)
  area<-addObjectToArea(area,Res2)
  area<-addObjectToArea(area,D1)
  area<-addObjectToArea(area,D2)
  area<-addObjectToArea(area,D3)
  area<-addObjectToArea(area,J1)
  \dontrun{
    plot(area)
  }
  simulated<-sim(area)
  observed<-apply(simulated$operation$operation$junctions[[1]]$operation$outflow,1,sum)
  observed<-observed+rnorm(length(observed),mean(observed)*0.2,sd(observed)*0.1)
  GOF(simulated,J1,observed)
}

\seealso{\code{\link[WRSS]{sim}}}

\keyword{ list }