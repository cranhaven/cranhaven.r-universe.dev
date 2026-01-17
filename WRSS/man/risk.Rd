\name{risk}
\alias{risk}
\title{
risk-based criteria 
}
\description{
this function returns risk-based criteria for demand site(s) built-in the object inherited from class of \code{sim}.
}
\usage{
risk(object , s.const = 0.95)
}
\arguments{
  \item{object}{an object from class of \code{sim}}
  \item{s.const}{satisfactory constant: a value in [0, 1] interval, which refers to the level at which if a demand is supplied over the \code{s.const} is considered fully supplied.}
}
\details{
This function computes the riks criteria based on the formulations proposed by Hashimoto et.al (1982).
}
\value{
a matrix of criteria
}
\references{
Hashimoto, Tsuyoshi, Jery R. Stedinger, and Daniel P. Loucks. "Reliability, resiliency, and vulnerability criteria for water resource system performance evaluation." Water resources research 18.1 (1982): 14-20.
}
\author{
Rezgar Arabzadeh
}
\seealso{\code{\link[WRSS]{sim}}}
\examples{
Res<-createReservoir(name="R1",type='storage',
                     netEvaporation=rnorm(120,0.5,0.1),
                     geometry=list(deadStorage= 10,
                                   capacity= 700,
                                   storageAreaTable= cbind(seq(0,900,100),seq(0,9,1))))
R<-createRiver(name="Riv1",downstream=Res,discharge=rnorm(120,500,4))
waterVariation<-round(sin(seq(0,pi,length.out=12))*
                       100/sum(sin(seq(0,pi,length.out=12))))
D1<-createDemandSite(name ="D1",
                     demandParams=list(waterUseRate=5,
                                       waterVariation=waterVariation,
                                       cropArea=500),
                     suppliers=list(Res),priority=1)
D2<-createDemandSite(name ="D2",
                     demandParams=list(waterUseRate=5,
                                       waterVariation=waterVariation,
                                       cropArea=500),
                     suppliers=list(Res),priority=2)
D3<-createDemandSite(name ="D3",
                     demandParams=list(waterUseRate=5,
                                       waterVariation=waterVariation,
                                       cropArea=500),
                     suppliers=list(Res),priority=3)
area<-createArea(simulation=list(start='2000-01-01',end='2009-12-29',interval='month'))
area<-addObjectToArea(area,R)
area<-addObjectToArea(area,Res)
area<-addObjectToArea(area,D1)
area<-addObjectToArea(area,D2)
area<-addObjectToArea(area,D3)
risk(sim(area))
}
\keyword{ matrix }
