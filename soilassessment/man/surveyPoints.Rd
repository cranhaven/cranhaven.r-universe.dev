\name{surveyPoints}
\alias{surveyPoints}
\title{
A function to generate georeferenced locations for monitoring soil conditions
}
\description{
This function uses stratified random sampling to generate georeferenced locations for monitoring soil conditions
}
\usage{
surveyPoints(soilmap,scorpan,conditionclass,mapproportion)
}
\arguments{
  \item{soilmap}{
input classified map of soil condition
}
  \item{scorpan}{
number of scorpan factors that generated teh soil condition map. The range is 1-5
}
  \item{conditionclass}{
reference class in the soil condition map to be monitored. The class code should be in the map
}
  \item{mapproportion}{
Proportion in percent of the reference class in the soil condition map to be monitored.
}
}
\details{
The number of scorpan factors can be assumed but need to be with respect to the soil forming factors.The maximum possible number of factors is 5 irrespective of number of layers in each factor while the minimum number is 1.The soil condition class is the class code in the map which is to be targeted
}
\value{
A spatial points dataframe with projection similar to the soil condition map projection
}
\author{
Christian Thine Omuto
}
\seealso{
featureRep, imageIndices, pedoTransfer, classCode
}
\examples{
library(sp)
library(raster)
ec=suitabinput["ec"]
ph=suitabinput["ph"]
soc=nutrindicator["soc"]
clay=textureinput["clay"]
texture=suitabinput["texture"]
newmap=ec
newmap$ph=ph$ph
newmap$ECe=ECconversion1(ec$ec*0.1,texture$texture,"FAO","1:5", soc$soc,clay$clay)
newmap$salt=saltSeverity(newmap$ECe,newmap$ph,0.84,"FAO")
newmap$salineclass=classCode(newmap$salt,"saltseverity")
newmap$salineclass1=as.factor(newmap$salineclass)
spplot(newmap["salineclass"], main="Salinity Code")
summary(newmap$salt)
summary(newmap$salineclass)
salt=raster(newmap["salt"])
salt1=newmap["salt"]
n_points=surveyPoints(salt1,4,11,80)
length(n_points$new)
spplot(salt1, scales=list(draw=TRUE),sp.layout=list("sp.points",n_points,pch=8,col="cyan"))
spplot(salt, scales=list(draw=TRUE),sp.layout=list("sp.points",n_points,pch=8,col="cyan"))
}
