\name{conformProjections}
\alias{conformProjections}
\title{ Getting conformed projections
}
\description{ Getting a conformed projection for a set of \code{\link[sp:Spatial-class]{Spatial}}* elements
        necessary for interpolation in the \code{\link{intamap-package}}.}
\usage{ 
conformProjections(object)
}
\arguments{
\item{object}{ an object of the type described in \code{\link{intamap-package}}}
} 

\details{
\code{conformProjections} is a function that attempts to reproject all projected 
elements in \code{object} to one common projection. 
The function is usually called with an intamap object as argument 
from \code{\link{createIntamapObject}} if the parameter \code{confProj = TRUE}. 
Thus it is a function that is usually not necessary to call separately.

The need for this function is because 
several of the functions in a typical spatial interpolation work flow inside the 
\code{\link{intamap-package}} require that the elements have a common projection. In 
addition, there are some functions which are not able to deal with 
unprojected spatial objects, i.e. objects with coordinates given in lattitude and
longitude. \code{conformProjections} will hence also attempt to reproject all 
elements that 
have coordinates in lattitude and longitude, even in the cases where they 
all have the same projections.

If only one of observations or predictionLocations has a projection (or is longlat), 
the other one is assumed to be equal. A warning is issued in this case.

The common projection depends on the object that is passed to conformProjections.
First of all, if \code{intCRS} (see below) is present as an element of the object, all elements
will be reprojected to this projection. If not, \code{intCRS} will be set equal to 
the first projection possible in the list below.

\describe{
 \item{intCRS}{Can be given as a component in \code{object} - and is the 
               user-defined common projection used for interpolation}
 \item{targetCRS}{Can be given as a component in \code{object} - and is 
               the user-defined target projections}
 \item{predCRS}{The projection of the predictionLocations in \code{object}}
 \item{obsCRS}{The projection of the observations} 
% This is the former version, where epsg:3035 was used as default.
% \item{defaultCRS}{A default projection is used when all the components have 
%                coordinates in lattitude and longitude, and no interpolation
%                or target projection is given. The chosen projection is
%                CRS("+init=epsg:3035") which is suitable for European data,
%                but not necessarily for data from other regions.}
}

}

\value{ 
A list of the parameters to be included in the \code{object} described in \code{\link{intamap-package}}}

\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\author{ Jon Olav Skoien }
\examples{
data(meuse)
coordinates(meuse) = ~x+y
proj4string(meuse) <- CRS("+proj=stere +lat_0=52.15616055555555 
    +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m")

predictionLocations = spsample(meuse, 50, "regular")

krigingObject = createIntamapObject(
	observations = meuse,
	predictionLocations = predictionLocations,
  formulaString = as.formula("log(zinc)~1"),
  intCRS = "+init=epsg:3035"
	)

krigingObject = conformProjections(krigingObject)
proj4string(meuse)
proj4string(krigingObject$observations)
}
\keyword{spatial}
