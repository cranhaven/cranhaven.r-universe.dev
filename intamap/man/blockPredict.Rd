\name{blockPredict}
\alias{blockPredict}
\alias{spatialPredict.block}
\title{ Spatial block prediction}
\description{\code{blockPredict} is a generic method for prediction of 
spatially aggregated variables within the \code{\link{intamap-package}} package.
}
\usage{ blockPredict(object, ...) }
\arguments{
  \item{object}{ a list object of the type described in \code{\link{intamap-package}}}
  \item{...}{ other arguments that will be passed to the requested interpolation method. 
    See the individual interpolation methods for more information. The following arguments 
    from \code{object} can be overrun through \code{...}:
    \describe{
      \item{\code{block}}{Block size; a vector with 1, 2 or 3 values containing the size
                  of a rectangular in x-, y- and z-dimension respectively
                  (0 if not set), or a data frame with 1, 2 or 3 columns,
                  containing the points that discretize the block in the
                  x-, y- and z-dimension to define irregular blocks relative to
                  (0,0) or (0,0,0) - see also the details section of \code{\link[gstat]{predict.gstat}}.
                  By default, predictions or simulations refer to the support of the data values. }
      \item{\code{cellsize}}{size of cells for discretization of blocks for numerical simulation}
    }
  } 
} 

\details{
The function \code{blockPredict} is a wrapper around the \code{spatialPredict.block} function 
within the \code{\link{intamap-package}} package, to simplify the calls for block predictions. 

Block predictions are spatial predictions assumed to be valid for a certain area. 
The blocks can either be given by passing \code{\link[sp:SpatialPolygons]{SpatialPolygons}} as the 
predicitonLocations or by passing the block-argument through the parameters of the 
object or through the \code{...}-argument.

There are esentially two ways to solve the problems of block predictions.
\describe{
  \item{analytical}{block predictions can be found directly by block kriging}
  \item{numerical}{block predictions can be found through numerical simulations
   over a set of points within the block, the requested output is found by 
   averaging over these simulations }
}

The analytical solutions are used when applicable. This is typically for 
ordinary kriging based methods and prediction types that can be
found by linear aggregation (e.g. block mean). 

If the prediction type necessitates simulations, this is done by subsampling
the blocks. This can either be done block-wise, with a certain number of points
within each block, with a certain cellsize, or with a certain number of points




\code{automap} Uses function \code{\link[automap]{autoKrige}} in the 
\code{automap} package.
If \code{object} already includes a variogram model, 
\code{\link[gstat]{krige}} in the \code{gstat}-package will be called directly.
}

\value{ 
a list object similar to \code{object}, but extended with predictions at
a the set of locations defined \code{object}.
}

\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\author{ Jon Olav Skoien}
\seealso{
\code{\link[gstat]{gstat}},\code{\link[automap]{autoKrige}}
}
\examples{
# This example skips some steps that might be necessary for more complicated
# tasks, such as estimateParameters and pre- and postProcessing of the data
data(meuse)
coordinates(meuse) = ~x+y
meuse$value = log(meuse$zinc)
data(meuse.grid)
gridded(meuse.grid) = ~x+y
proj4string(meuse) = CRS("+init=epsg:28992")
proj4string(meuse.grid) = CRS("+init=epsg:28992")

# set up intamap object:
obj = createIntamapObject(
	observations = meuse,
	predictionLocations = meuse.grid[sample(1:length(meuse.grid),10),],
	targetCRS = "+init=epsg:3035",
	class = "automap"
)

# do interpolation step:
obj = conformProjections(obj)
obj = estimateParameters(obj) 
obj = blockPredict(obj,block=c(100,100))  # blockPredict


# intamap object for which simulation is needed:
meuse$value = meuse$zinc
obj = createIntamapObject(
	observations = meuse,
	predictionLocations = meuse.grid[sample(1:length(meuse.grid),5),],
	params = list(ngrid = 16),
  class = "transGaussian"  # trans-Gaussian kriging method
)
obj = estimateParameters(obj, lambda = 0)  # lambda is optional, lambda = 0 gives lognormal kriging
obj = blockPredict(obj,block=c(100,100))  # blockPredict
}

\keyword{spatial}
