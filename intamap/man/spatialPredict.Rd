\name{spatialPredict}
\alias{spatialPredict}
\alias{spatialPredict.automap}
\alias{spatialPredict.copula}
\alias{spatialPredict.default}
\alias{spatialPredict.idw}
\alias{spatialPredict.linearVariogram}
%\alias{spatialPredict.psgp}
\alias{spatialPredict.transGaussian}
\alias{spatialPredict.yamamoto}
\title{ Spatial prediction}
\description{\code{spatialPredict} is a generic method for spatial predictions 
within the \code{\link{intamap-package}}.
A series of methods have been implemented, 
partly based on other R-packages (as \code{\link[gstat:krige]{krige}}),
other methods have been developed particularly for the INTAMAP project. The object 
has to include a range of variables, further described in 
\code{\link{intamap-package}}. The prediction method is 
chosen based on the class of the object.
}
\usage{
\method{spatialPredict}{automap}(object, nsim = 0, ...)
\method{spatialPredict}{copula}(object, ...)
\method{spatialPredict}{default}(object, ...)
\method{spatialPredict}{idw}(object, ...)
\method{spatialPredict}{linearVariogram}(object, nsim = 0, ...)
%\method{spatialPredict}{psgp}(object, nsim =0, ...)
\method{spatialPredict}{transGaussian}(object, nsim = 0, ...)
\method{spatialPredict}{yamamoto}(object, nsim = 0, ...)
}
\arguments{
\item{object}{ a list object. Most arguments necessary for interpolation
are passed through this object. See \code{\link{intamap-package}} for further 
description of the necessary content of this variable}
\item{nsim}{number of simulations to return, for methods able to return simulations}
\item{...}{ other arguments that will be passed to the requested interpolation method. 
See the individual interpolation methods for more information. }
} 

\details{
The function \code{spatialPredict} is a wrapper around different
spatial interpolation methods found within the \code{\link{intamap-package}} or within other packages
in \code{R}. It is for most of the
methods necessary to have parameters of the correlation structure
included in \code{object} to be able to carry out the spatial prediction.
Below are some details 
about particular interpolation methods


\describe{
\item{\code{default}}{a default method is not really implemented, this function is only 
              created to give a sensible error message if the function is called
              with an object for which no method exist}

\item{\code{automap}}{If the object already has an element \code{variogramModel} with
  variogram parameters,
  \code{\link[gstat]{krige}} is called. If the this is not a part of the object,
  \code{\link{estimateParameters}} is called to create this element.}

\item{\code{copula}}{spatial prediction using \code{\link{bayesCopula}}}

\item{\code{idw}}{applies inverse distance modelling with the idp-power found by \code{estimateParameters.idw}}

\item{\code{linearVariogram}}{this function estimates the process using an unfitted linear variogram; although variance is returned it can not be relied upon}

  
%\item{\code{psgp}}{spatial prediciton using the \code{\link[psgp]{makePrediction}} in the \emph{psgp} package}

\item{\code{transGaussian}}{spatial prediction using \code{\link[gstat:krigeTg]{krigeTg}}}

\item{\code{yamamoto}}{spatial prediction using \code{\link{yamamotoKrige}}}

It is also possible to add to the above methods with functionality from
other packages, if wanted. 
You can also check which methods are available from other packages by calling 
\preformatted{
>methods(spatialPredict)
}
}

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
\code{\link[gstat]{gstat}},\code{\link[automap]{autoKrige}}, 
\code{\link{createIntamapObject}}, \code{\link{estimateParameters}}, 
\code{\link{intamap-package}}

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
	predictionLocations = meuse.grid,
	targetCRS = "+init=epsg:3035",
	params = getIntamapParams(),
	class = "linearVariogram"
)

# do interpolation step:
obj = spatialPredict(obj)  # spatialPredict.linearVariogram
}
\keyword{spatial}
