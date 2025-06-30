\name{biasCorr}
\alias{biasCorr}
\title{Bias correction}
\description{
Identifies and removes biases from measurement networks
}
\usage{
biasCorr(object,regCode = "regCode",...)
}
\arguments{
\item{object}{ Data frame with observations with same format as \code{observations} 
               described in the presentation of the 
               \code{\link[intamap:intamap-package]{intamap-package}})}
\item{regCode}{the column name of regions in the data polygons, if existing}
\item{...}{further arguments to the bias correction methods called, see details below}
}


\details{
Many data sets can consist of data coming from a large number of different measurement
networks, using different measurement devices or applying different methods for 
post-processing the observations. Some of these networks can exist in the same area,
e.g. when different authorities are measuring the same, but at different locations
(one of them in cities, the other one close to lakes), some networks will only
exist as neighbouring networks (networks operated by a municipality or a country).
Local networks can also be grouped together as one national data-base, which can again
be merged into an international data-base.

One challenge with the merging into data-bases is that there will be inconsistencies 
between measurements in the different networks, which will again cause difficulties
when attempting to map the observations, as done in the intamap-package. The intention
of this function is therefore to call other functions that are able to identify
and remove such differences, which can be referred to as biases between the networks.

There are at the moment two methods available for bias correction, "UK" and "LM". 
"UK" is a universal kriging based approach implemented in 
\code{\link{findBiasUK}}. This method can only deal with biases between 
neigbouring networks, but is well capable of taking covariates into account.
"LM" is based on local methods for estimating differences between networks, and
is implemented in \code{\link{findLocalBias}} and \code{\link{findRegionalBias}}.
The choice between the methods is given by the parameter \code{biasRemovalMethod}
in the parameter element of the object, set in \code{\link[intamap:getIntamapParams]{getIntamapParams}},
called from createIntamapObject.

The function will remove biases according to the settings of the parameters 
\code{removeBias}. 
Below is a list of the functions available for bias corrections. See each individual
function for more information about usage.

\describe{
\item{\link{findBiasUK}}{The universal kriging based function for 
    finding biases between neighbouring networks}
\item{\link{findLocalBias}}{Find biases for ovelapping networks}
\item{\link{removeLocalBias}}{Removes biases between ovelapping networks}
\item{\link{findBoundaryLines}}{Find points that define adjacent boundaries
            between regions}
\item{\link{findRegionalBias}}{Find biases for neighbouring networks}
\item{\link{removeRegionalBias}}{Remove biases between neighbouring networks}
}
}

\value{
Data frame with observations, with the identified biases removed.
}

\references{

Skoien, J. O., O. P. Baume, E. J. Pebesma, and G. B. M. Heuvelink. 2010. 
Identifying and removing heterogeneities between monitoring networks. 
Environmetrics 21(1), 66-84.
}
\author{ Jon Olav Skoien}

\seealso{
\code{\link{findLocalBias}}
}
\examples{
\donttest{
data(meuse)
data(meuse.grid)
observations = data.frame(x = meuse$x,y = meuse$y,value = log(meuse$zinc))
coordinates(observations) = ~x+y
gridded(meuse.grid) = ~x+y
pBoundaries = spsample(observations, 8, "regular",bb = bbox(observations) +  
              matrix(c(-400,-400,400,400),ncol=2),offset=c(0,0))
gridded(pBoundaries) = TRUE
cs = pBoundaries@grid@cellsize[1]/2
dx = cs/5

Srl = list()
nb = dim(coordinates(pBoundaries))[1]
for (i in 1:nb) {
  pt1 = coordinates(pBoundaries)[i,]
  x1 = pt1[1]-cs
  x2 = pt1[1]+cs
  y1 = pt1[2]-cs
  y2 = pt1[2]+cs

  boun = data.frame(x=c(seq(x1,x2,dx),rep(x2,11),seq(x2,x1,-dx),rep(x1,11)),
                    y=c(rep(y1,11),seq(y1,y2,dx),rep(y2,11),seq(y2,y1,-dx)))
  coordinates(boun) = ~x+y
  boun = Polygon(boun)
  Srl[[i]] = Polygons(list(boun),ID = as.character(i))
}
pBoundaries = SpatialPolygonsDataFrame(SpatialPolygons(Srl),
                                      data = data.frame(ID=c(1:nb)))
observations$ID = over(observations, geometry(pBoundaries))
blines = findBoundaryLines(pBoundaries,regCode = "ID")


object = createIntamapObject(observations,meuse.grid,boundaryLines = blines, 
  params = list(removeBias = "regionalBias"))
object = biasCorr(object,regCode= "ID")
object$regionalBias$regionalBias
pBoundaries$bias = NA
pBoundaries$bias[object$regionalBias$regionalBias$ID] = object$regionalBias$regionalBias$ols
spplot(pBoundaries,"bias",sp.layout = list(list("sp.points",observations)))
}
}
\keyword{spatial}
