\name{interpolate}
\alias{interpolate}
\alias{interpolateBlock}
\title{ spatial interpolation }
\description{\code{interpolate} is a function that interpolates spatial data }
\usage{
interpolate(observations, predictionLocations, 
            outputWhat = list(mean = TRUE, variance = TRUE), 
            obsChar = NA, methodName = "automatic", maximumTime = 30, 
            optList = list(), cv = FALSE, ...)
interpolateBlock(observations, predictionLocations, outputWhat, 
            blockWhat = "none", obsChar = NA, methodName = "automatic", 
            maximumTime = 30, 
            optList = list(), ...)
}
\arguments{
\item{observations}{observation data, object of class \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}}. 
                    The observation to be interpolated has to be identified through the 
                    column name \code{value}}
\item{predictionLocations}{ prediction locations, object of class \code{\link[sp:SpatialPoints]{SpatialPoints}}*, \code{\link[sp:SpatialPixels]{SpatialPixels}}* or \code{\link[sp:SpatialGrid]{SpatialGrid}}*, or the number of predictionLocations to be sampled from the boundaries of the observations}
\item{outputWhat}{ list with names what kind of output is expected, e.g.
                  \code{outputWhat = list(mean=TRUE,variance=TRUE, nsim = 5)}}
\item{blockWhat}{List defining particular output for block predictions. See \code{\link{createIntamapObject}}}
\item{obsChar}{list with observation characteristics, used by some interpolation methods}
\item{methodName}{name of interpolation method to be used, see \link{spatialPredict} for more details, or \code{automatic}, to let
                  the method be decided by the program, based on maximumTime and type of variables input}
\item{maximumTime}{the maximum time available for interpolation, will be compared to the result of \code{\link{predictTime}} 
                   for the requested method, or for finding the best interpolation method
                   able to finish within this time}
\item{optList}{list; further options, mainly passed to \code{\link{createIntamapObject}}
                  as the argument \code{params}, directly as arguments, but some are used
                  locally in \code{interpolate} and \code{interpolateBlock}:
                     \describe{
                       \item{formulaString}{passed as argument to \code{\link{createIntamapObject}},
                            if no formulaString is given, it will default to
                            \code{value~1} if \code{observations} has a column named
                            \code{value} or to \code{col1 ~ 1} where \code{col1}
                            is the first column of the observations }
                       \item{set.seed}{the possibility to pass a seed value to 
                            \code{interpolate}, to assure reproducible results also
                            for methods relying on random numbers}
                       \item{methodParameters}{methodParameters from an earlier run, see also \code{\link{methodParameters}}}
                       \item{model}{defining which variogram model(s) to fit when \code{methodName = "automap"}}
                     }                  
               }
\item{cv}{If cross-validation should be done}
\item{...}{other arguments to be passed to other functions}
} 



\value{ 
An intamap object, which is a list with elements, see \code{\link{intamap-package}}. 
The exact number and names of these elements might vary due to different methods applied,
but the list below shows the most typical:

 \item{observations}{the observations, as a \code{Spatial*DataFrame}}
 \item{predictionLocations}{the prediction locations, as a \code{\link[sp:Spatial-class]{Spatial}}-object}
 \item{formulaString}{the relationship between independent and dependent variables, 
       \code{value} or \code{obs} used if not given}
 \item{outputWhat}{a list of the prediction types to return}
 \item{anisPar}{the estimated anisotropy parameters}

 \item{variogramModel}{the estimated parameter for the method, can also be e.g. \code{copulaParams}
       for the copula method or \code{inverseDistancePower} for inverse distance power method.}
 \item{methodParameters}{a string, that when parsed, can be used to regenerate
       the variogram model or copula parameters. Useful for repeated calls to interpolate
       when it is not necessary to reestimate the parameters.}
 \item{predictions}{a \code{Spatial*DataFrame} with predictions, for most 
       methods with a format equal to the output from \code{\link[gstat]{krige}} with 
       predicted mean and variance as \code{var1.pred} and \code{var1.var}}
 \item{outputTable}{a matrix, organized in a convenient way for the calling WPS;
     first row: x-coordinates,  second row: y-coordinates; further rows: 
     output elements as specified by \code{outputWhat}}
 \item{processDescription}{some textual descriptions of the interpolation
        process, including warnings}

}

\details{
The functions \code{interpolate} and \code{interpolateBlock}  are particularly implemented for being called by 
a Web Processing Server (WPS), but they can also be used interactively. The only necessary 
arguments are \code{observations} and \code{predictionLocations}. It is also recommended
to set \code{outputWhat}, and \code{blockWhat} if necessary. If \code{outputWhat}
contains \code{nsim}, the return table will also contain a number of realisations,
for methods able to return simulations. 
%These are at the moment \code{automap} and \code{psgp}.

\code{interpolate} can use different interpolation methods for the result. The function
will internally call the following functions which can be method specific.
\itemize{
\item \code{\link{preProcess}}
\item \code{\link{estimateParameters}}
\item \code{\link{spatialPredict}}
\item \code{\link{postProcess}}
}
An indication of available methods can be given by \code{methods(estimateParameters)}
or \code{methods(spatialPredict)}.
The method can be set through the argument \code{methodName}, or through the 
built-in automatic selection method. There are different criteria that helps 
in selecting the right method for a particular data set. There are four 
methods that are available for the automatic choice:
\code{automap}, \code{psgp} (from the separate package \code{psgp}) 
\code{copula} and \code{transgaussian} are the possibilities. 
First of all, if observation errors are present, the \code{psgp} method is preferred. 
If not, it is checked whether the data appear to deviate significantly from normality. 
This is assumed to be the case if any of the tests below are TRUE:

\preformatted{
      test[1] = length(boxplot.stats(dataObs)$out)/length(dataObs) > 0.1
      test[2] = fivenum(dataObs)[3] - fivenum(dataObs)[2] < IQR(dataObs)/3
      test[3] = fivenum(dataObs)[4] - fivenum(dataObs)[3] < IQR(dataObs)/3
      g = boxcox(dataObs ~ 1,lambda=seq(-2.5,2.5,len=101),plotit=FALSE)$y
      test[4] = g[71] < sort(g)[91]
}
where \code{\link{fivenum}} defines the Tukey five number statistic and 
\code{\link{IQR}} finds the interquartile range of the data. If the minimum of 
dataObs is <= 0, min(dataObs) + sdev(dataObs) is added to all values.
At last, the function calls \code{\link{predictTime}} for an estimate of the 
prediction time. If any of the tests above were true and the estimated prediction time
for \code{copula} prediction is below \code{maximumTime}, the \code{copula}
 method is chosen. If any of the
tests were TRUE and the estimated prediction time is too long, \code{transGaussian} 
kriging is chosen, as long as all values are above zero. If any of 
the tests are true for a set of observations with negative or zero-values, 
\code{automap} is chosen, but a warning is issued.

The element \code{methodParameters} in the object being returned is a string that makes it possible
to regenerate the variogram model or the copula parameters in \code{\link{createIntamapObject}}.
This is particularly useful when the function is called through a WPS, when
the element with the estimated parameters cannot be preserved in a state 
that makes it possible to use them for a later call to \code{interpolate}.

The possibility
for doing parallel processing is enabled for some of the main methods.
To be able to take advantage of multiple CPUs on a computer, the package
\code{doParallel} must be downloaded, additionally the parameter nclus must be set to
a value larger than 1. Parallel computation is not taken into account when 
estimating the prediction times.

}

\seealso{
\code{\link{createIntamapObject}}, \code{\link{estimateParameters}}, 
\code{\link{spatialPredict}}, \code{\link{intamap-package}}
}
\references{ 

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}
\author{ Edzer Pebesma }

\examples{
\donttest{
data(meuse)
coordinates(meuse) = ~x+y
meuse$value = meuse$zinc
data(meuse.grid)
gridded(meuse.grid) = ~x+y
x = interpolate(meuse, meuse.grid, list(mean=TRUE, variance=TRUE))
summary(t(x$outputTable))}
}
\keyword{spatial}
