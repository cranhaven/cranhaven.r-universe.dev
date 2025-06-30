\name{methodParameters}
\alias{methodParameters}
\alias{methodParameters.default}
\alias{methodParameters.copula}
\alias{methodParameters.idw}
\title{ generate string for generation of method parameters }
\description{function that generates a parsable string of identified method parameters 
  for an intamap interpolation object}
\usage{
\method{methodParameters}{default}(object)
\method{methodParameters}{copula}(object)
\method{methodParameters}{idw}(object)
}
\arguments{
\item{object}{  a list object. Most arguments necessary for interpolation
are passed through this object. See \code{\link{intamap-package}} for further 
description of the necessary content of this variable}
} 

\value{
A string that, when parsed, will recreate the methodParameters
}

\details{
The function creates a text-string that makes it possible to add the 
the method parameters (anisotropy and idw-parameter, variogram model or 
copula parameters) to the \code{object} in a later call to 
\code{\link{createIntamapObject}} or \code{\link{interpolate}} 
without having to re-estimate the parameters.
This function is particularly useful when \code{\link{interpolate}} is
called from a Web Processing Service, and the user wants to reuse the 
method parameters. The function is mainly assumed to be called from 
within \code{\link{interpolate}}.

The default method assumes a variogram model of \emph{gstat} type, e.g. a variogram
similar to what can be created with a call to \code{\link[gstat]{vgm}}.
Also \emph{psgp} uses this variogram model.
}


\references{ 
Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 
}

\author{ Jon Olav Skoien}

\examples{
sessionInfo()
data(meuse)
coordinates(meuse) = ~x+y
meuse$value = log(meuse$zinc)
# set up intamap object:
krigingObject = createIntamapObject(
		observations = meuse,
		formulaString = as.formula('value~1'),class = "automap")
# do estimation steps:
krigingObject = estimateParameters(krigingObject)
krigingObject = methodParameters(krigingObject)

# Create a new object
krigingObject2 = createIntamapObject(observations = meuse,
		formulaString = as.formula('value~1'),
    params = list(methodParameters = krigingObject$methodParameters))

krigingObject$variogramModel
krigingObject2$variogramModel

}


\keyword{spatial}
