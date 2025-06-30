\name{createIntamapObject}
\alias{createIntamapObject}
\title{Create an object for interpolation within the intamap package}
\description{
This is a help function for creating an object (see \code{\link{intamap-package}}
 to be used for interpolation within the \code{\link{intamap-package}}
}
\usage{createIntamapObject(observations, obsChar, formulaString,
            predictionLocations=100, targetCRS, boundaries, boundaryLines,
            intCRS, params=list(), boundFile, lineFile, class="idw",
            outputWhat, blockWhat = "none",...)
}

\arguments{
\item{observations}{a 
                 \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}}, 
                 \code{\link[sp:SpatialPixelsDataFrame]{SpatialPixelsDataFrame}}, 
                 \code{\link[sp:SpatialPixelsDataFrame]{SpatialGridDataFrame}},
                 \code{\link[sp:SpatialLines]{SpatialLinesDataFrame}} or
                 \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} with observations. Note that there
                 are only few methods that can actually handle interpolation of
                 observations with a support}
\item{obsChar}{list with observation characteristics, used by some interpolation methods}
\item{formulaString}{formula that defines the dependent variable as a linear model 
      of independent variables; suppose the dependent variable has name \code{z}, 
      for ordinary and simple kriging use the formula \code{z~1}; 
      for universal kriging, suppose \code{z} is linearly dependent on 
      \code{x} and \code{y}, use the formula \code{z~x+y}. The formulaString defaults
      to \code{"value~1"} if \code{value} is a part of the data set. 
      If not, the first column of the data set is used.}
\item{predictionLocations}{either a \code{\link[sp:Spatial-class]{Spatial}}* object with 
                 prediction locations or an integer with the requested number
                 of prediction locations. If \code{boundaries} are supported, 
                 the sampled prediction locations will be sampled within the 
                 \code{boundaries} }
\item{targetCRS}{the wanted projection for the interpolated map}
\item{boundaries}{\code{\link[sp:SpatialPixelsDataFrame]{SpatialPolygonsDataFrame}} with the boundaries
                of regions in the prediction region}
\item{boundaryLines}{\code{\link[sp:SpatialPixelsDataFrame]{SpatialPointsDataFrame}} with the boundaries between
                     pairs of regions discretized as points. 
                     Will be read from file if \code{lineFile} is 
                     given or will be created from \code{boundaries} if not.}
\item{intCRS}{a particular projection requested for the interpolation}
\item{params}{parameters for the interpolation, given as exceptions to the 
              default parameters set in the function \code{\link{getIntamapParams}}. It is also possible to pass
              a \code{methodParameters} from an earlier call, as defined from the function
              \code{\link{methodParameters}}. }
\item{boundFile}{Filename where boundaries can be found, e.g. a shapefile }
\item{lineFile}{Filename where paired points on boundaries can be found}
\item{class}{setting the class(es) of the object, see \code{\link{intamap-package}}}
\item{outputWhat}{List defining the requested type of output. Parameters:
  \describe{
    \item{\code{mean = TRUE}}{Usual kriging prediction }
    \item{\code{variance = TRUE}}{Usual kriging error }
    \item{\code{quantile}}{The estimated quantile for a certain threshold}
    \item{\code{excprob}}{Exceedance probability for a certain threshold}
    \item{\code{cumdistr}}{The cumulative distribution for a certain value}
    \item{\code{MOK}}{Assumed unbiased prediction using the MOK method for the threshold given.
               See \code{\link{unbiasedKrige}}}
    \item{\code{IWQSEL}}{Assumed unbiased prediction using the IWQSEL method for the threshold given.
               See \code{\link{unbiasedKrige}}}
    \item{\code{...}}{Additional prediction types that do not exist in the default parameter set,
           particularly parameters necessary for new methods within the 
           \code{\link{intamap-package}}.}
   }
   The list defaults to list (mean = TRUE) for objects of class IDW and 
   list(mean=TRUE, variance = TRUE) for all other objects.
}
\item{blockWhat}{List defining particular output for block predictions. These include:
  \describe{
    \item{\code{blockMax}}{logical; whether to predict maximum within block, if block predictions}
    \item{\code{blockMin}}{logical; whether to predict mimimum within block, if block predictions}
    \item{\code{fat}}{Prediction of area within block above a \code{threshold} (\code{fat = threshold}}
    \item{\code{blockMaxVar}}{logical; whether to predict the variance of the prediction of max within the block, 
                               similarly it is possible to set
                              \code{blockMinVar = TRUE} and \code{fatVar = threshold}}
  }
}
\item{...}{ 
\itemize{
  \item Either: other elements that can be used by particular interpolation methods. 
  These are added to the object as named elements.
  \item Or: elements that have been created 
   in earlier calls to one of the functions in the \code{\link{intamap-package}}, 
   and that are
   not supposed to change in the second call. By adding these
   elements to the object in \code{createIntamapObject}, they can be reused 
   without having to re-estimate them. Typical examples are the elements created 
   from a call to \code{\link{preProcess}}
} 
}
}

\details{
This function is a help function for creating an object (see
\code{\link{intamap-package}}) for interpolation within the 
\code{\link{intamap-package}}.
The function uses some default values if certain elements are not included.


If \code{createIntamapObject} is called without predictionLocations, or if a number
is given, the function will sample a set of predictionLocations. These will
be sampled from a regular grid.

targetCRS and intCRS are not mandatory variables, but are recommended if the 
user wants predictions of a certain projection. intCRS is not necessary if
the targetCRS is given and has a projection (is not lat-long). It is recommended
to include the argument intCRS if all projected elements are lat-long, as many of 
the interpolation methods do not work optimal with lat-long data. 

The ...-argument can be used for arguments necessary for new methods not being 
a part of the \code{\link{intamap-package}}.  It is also a method for reusing previously calculated
elements that can be assumed to be unchanged for the second interpolation.  
%One example of such elements could be biases estimated in 
%\code{biasCorr} in the separate \code{intamapInteractive}-package.
}

\value{ 
An object with observations, prediction locations, parameters and possible
additional elements for automatic interpolation. The object will have class
equal to the value of argument \code{class}, and methods in the 
\code{\link{intamap-package}} will dispatch on the
object according to this class.
}

\seealso{
\code{\link{intamap-package}} and \code{\link{getIntamapParams}}
}

\references{ 
Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 
}

\author{ Jon Olav Skoien }
\examples{
# set up data:
data(meuse)
coordinates(meuse) = ~x+y
meuse$value = log(meuse$zinc)
data(meuse.grid)
gridded(meuse.grid) = ~x+y
proj4string(meuse) = CRS("+init=epsg:28992")
proj4string(meuse.grid) = CRS("+init=epsg:28992")

# set up intamap object:
idwObject = createIntamapObject(
	observations = meuse,
	predictionLocations = meuse.grid,
	targetCRS = "+init=epsg:3035",
	class = "idw"
)
}
\keyword{spatial}
