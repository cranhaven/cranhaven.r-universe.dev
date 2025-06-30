\name{estimateParameters}
\alias{estimateParameters}
\alias{estimateParameters.automap}
\alias{estimateParameters.copula}
\alias{estimateParameters.default}
\alias{estimateParameters.idw}
\alias{estimateParameters.linearVariogram}
%\alias{estimateParameters.psgp}
\alias{estimateParameters.transGaussian}
\alias{estimateParameters.yamamoto}
\title{ Automatic estimation of correlation structure parameters}
\description{
Function to estimate correlation structure parameters. The actual parameters
depend on the method used.
}
\usage{
\method{estimateParameters}{automap}(object, ... )
\method{estimateParameters}{copula}(object, ... )
\method{estimateParameters}{default}(object, ...)
\method{estimateParameters}{idw}(object, ... )
\method{estimateParameters}{linearVariogram}(object, ...)
%\method{estimateParameters}{psgp}(object, ... )
\method{estimateParameters}{transGaussian}(object, ... )
\method{estimateParameters}{yamamoto}(object, ... )
}
\arguments{
\item{object}{ an intamap object of the type described in \code{\link{intamap-package}}}
\item{...}{ other arguments that will be passed to the requested interpolation method. 
See the individual methods for more information. Some parameters that are particular for some methods:
\describe{
\item{\code{idpRange}}{ range of idp (inverse distance weighting power) values 
       over which to optimize mse (idw-method) }
\item{\code{nfolds}}{ number of folds in n-fold cross validation (idw-method)}
\item{\code{lambda}}{lambda parameter for \code{\link[MASS]{boxcox}}-transformation
      (transGaussian method)}
\item{\code{significant}}{logical; if TRUE only transform if any of the four tests 
            described under \code{\link{interpolate}} are TRUE (transGaussian method)}
}
}
} 


\details{
The function \code{estimateParameters} is a wrapper around different
methods for estimating correlation parameters to be used for the spatial 
prediction method \code{\link{spatialPredict}}. 
Below are some details about and/or links to the different methods currently implemented
 in the \code{\link{intamap-package}}.

\describe{
\item{\code{automap}}{It is possible but not necessary to estimate variogram parameters for 
  this method. If \code{estimateParameters} is called with an object of class \emph{automap},
  \code{\link[automap]{autofitVariogram}} will be called. 
  If \code{object} already includes a variogram model when 
  \code{\link{spatialPredict}} is called, 
  \code{\link[gstat]{krige}} in the \code{gstat}-package will be called directly.
  The user can submit an argument \code{model} with the model(s) to be fitted.  }

\item{\code{copula}}{finding the best copula parameters using \code{\link{copulaEstimation}}}

\item{\code{default}}{a default method is not really implemented, this function is only 
              created to give a sensible error message if the function is called
              with an object for which no method exist}

\item{\code{idw}}{fits the best possible idw-power to the data set by brute force searching within
the \code{idpRange}}

\item{\code{linearVariogram}}{this function just returns the original data, no parameter 
fitting is necessary for linear variogram kriging}

%\item{\code{psgp}}{finding the best variogram model using \code{\link[psgp]{learnParameters}} in the \emph{psgp} package}

\item{\code{transGaussian}}{Finding the best model parameters for transGaussian kriging 
         (\code{\link[gstat]{krigeTg}}). This means finding the best \code{lambda} for 
         the \code{\link[MASS]{boxcox}}-transformation and the fitted variogram
         parameters for the transformed variable. If \code{significant = TRUE}
         will \code{lambda} only be estimated
         if the data show some deviation from normality, i.e., that at least one
         of the tests described under \code{\link{interpolate}} is TRUE. Note that
         transGaussian kriging is only possible for data with strictly positive values.
         }

\item{\code{yamamoto}}{a wrapper around \code{estimateParameters.automap}, only to assure that there is a method
                     also for this class, difference to \code{automap} is more important in \code{\link{spatialPredict}}}

}
It is also possible to add to the above methods with functionality from
other packages, if wanted.
You can also check which methods are available from other packages by calling 
\preformatted{
>methods(estimateParameters)
}
}

\value{ 
a list object similar to \code{object}, but extended with correlation parameters.
}


\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\seealso{
\code{\link{createIntamapObject}}, \code{\link{spatialPredict}}, \code{\link{intamap-package}}
}
\author{ Jon Olav Skoien}
\examples{
set.seed(13131)

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
	formulaString=as.formula(zinc~1),
  predictionLocations = meuse.grid,
	class = "idw"
)

# run test:
checkSetup(idwObject)

# do interpolation steps:
idwObject = estimateParameters(idwObject, idpRange = seq(0.25,2.75,.25),
                               nfold=3) # faster
idwObject$inverseDistancePower
}
\keyword{spatial}