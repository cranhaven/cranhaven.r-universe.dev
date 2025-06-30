\name{findRegionalBias}
\alias{findRegionalBias}
\alias{removeRegionalBias}
\title{ Find and/or remove regional biases}
\description{ Method for identifying regional biases (in most cases biases between countries)}
\usage{
findRegionalBias(object,boundaryLines,
                 formulaString = value~1,
                 minKrige = 5, regCode = "regCode", unbias = "default")
removeRegionalBias(object, regionalBias, formulaString = value~1, regCode = "regCode")
}
\arguments{
\item{object}{an object of class \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}}, at least 
              containing observations and a regional identification code (\code{regCode})}  
\item{boundaryLines}{\code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}} with points defining the 
              boundaries between regions. This can be found using \code{\link{findBoundaryLines}}.}
\item{formulaString}{formula that defines the dependent variable as a linear model 
      of independent variables; suppose the dependent variable has name \code{z}, 
      for ordinary and simple kriging use the formula \code{z~1}; 
      for universal kriging, suppose \code{z} is linearly dependent on 
      \code{x} and \code{y}, use the formula \code{z~x+y}}              
\item{minKrige}{Setting a minimum number of observations necessary for kriging}
\item{regCode}{the column name of regions in the data polygons, if existing}
\item{unbias}{defines if a particular data dependent function should be used
              to set unbiasedness constraints for the biases. "default" gives
              one additional constraint, assuming that the average of the biases
              should be equal to zero. See also details below.}
\item{regionalBias}{ List of data frames, one for each region, each containing
                  biases for different networks in the region. }
} 


\details{
  This methods attempts to find biases between regional networks that are 
  separated by a boundary, based on  line kriging along these boundaries. 
  A typical example of such networks would be different national networks, 
  with the country borders as \code{boundaryLines}, but also other 
  boundaries can be considered. Further details can be found in Skoien et al. (2009).
  
  The parameter unbias can be used to name the unbiasedness function if 
  the user needs a different unbiasedness constraint than the default one.
  Such a function (with unbias = "new" above) should be similar to the following:
  
  \preformatted{
  unBias.new = function(cDiff,uRegCode) {
    D = cDiff$D
    Q = cDiff$Q
    V = cDiff$V
#
    D = rbind(D,0)
    cd = dim(D)[1]
    ino = which(uRegCode == "NO")
    iis = which(uRegCode == "IS")
    iuk = which(uRegCode == "UK" | uRegCode == "GB")
    if (length(iis) > 0) {
      D[cd,ino] = .5
      D[cd,iuk] = .5
      D[cd,iis]= -1
      Q[cd] = 0
      V[cd] = max(V)
      cd = cd+1
      D = rbind(D,0)
    }
    cd = cd + 1
    D = rbind(D,0)
    D[cd,] = 1
    Q[cd] = 0
    V[cd] = min(V)
    cDiff$D = D
    cDiff$Q = Q
    cDiff$V = V
    return(cDiff)
  }
  }
  The last part is similar to unbias.default. In the other part is solving the 
  problem where there are no boundaries between Iceland and any other 
  countries. This would cause a missing constraint when searching for the 
  biases, which will make it impossible to find a solution. The solution 
  here sets the bias for Iceland equal to the average of the bias 
  for Norway and United Kingdom. Note that the real bias for Iceland is not really
  estimated in this case, this construction is mainly to make sure that the 
  system can be solved. If one were only interested in the bias, it would in 
  this case be better to remove Iceland from the data set, as a real bias
  is not possible to find.

}

\value{ 
For \code{findRegionalBias}; a \code{\link[base]{data.frame}} with the biases for each country with uncertainty.

For \code{removeRegionalBias}; a \code{\link[base]{data.frame}} with observations, with biases removed
}

\references{ 

Skoien, J. O., O. P. Baume, E. J. Pebesma, and G. B. M. Heuvelink. 2010. 
Identifying and removing heterogeneities between monitoring networks.
Environmetrics 21(1), 66-84.

}
\author{ Jon Olav Skoien }
\examples{
library(intamapInteractive)
data(meuse)
observations = data.frame(x = meuse$x,y = meuse$y,value = log(meuse$zinc))
coordinates(observations) = ~x+y
pBoundaries = spsample(observations, 10, "regular",bb = bbox(observations) +  
              matrix(c(-400,-400,400,400),ncol=2),offset=c(0,0))
gridded(pBoundaries) = TRUE
cs = pBoundaries@grid@cellsize[1]/2

Srl = list()
nb = dim(coordinates(pBoundaries))[1]
for (i in 1:nb) {
  pt1 = coordinates(pBoundaries)[i,]
  x1 = pt1[1]-cs
  x2 = pt1[1]+cs
  y1 = pt1[2]-cs
  y2 = pt1[2]+cs

  boun = data.frame(x=c(x1,x2,x2,x1,x1),y=c(y1,y1,y2,y2,y1))
  coordinates(boun) = ~x+y
  boun = Polygon(boun)
  Srl[[i]] = Polygons(list(boun),ID = as.character(i))
}
pBoundaries = SpatialPolygonsDataFrame(SpatialPolygons(Srl),
                                      data = data.frame(ID=c(1:nb)))
observations$ID = over(observations, geometry(pBoundaries))
blines = findBoundaryLines(pBoundaries, regCode = "ID")
rb = findRegionalBias(observations, blines, value~1, regCode = "ID")
rb$regionalBias

obs2 = removeRegionalBias(observations, rb, value~1, regCode = "ID")



}
\keyword{spatial}
