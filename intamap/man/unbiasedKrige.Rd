\name{unbiasedKrige}
\alias{unbiasedKrige}
\title{ Unbiased kriging}
\description{\code{unbiasedKrige} is a function for modifying a kriging prediction
to a prediction that can be assumed to be unbiased for a certain threshold.}
\usage{
unbiasedKrige(object, formulaString, observations, predictionLocations,
              model, outputWhat, yamamoto, iwqmaxit = 500,
              iwqCpAddLim = 0.0001, debug.level, ...)
}
\arguments{
\item{object}{either an object of the intamap type (see \code{\link{intamap-package}} 
for further 
description of the necessary content of this variable) or the output from the function
\code{\link[gstat:krige]{krige}} in \code{\link[gstat:gstat]{gstat}}. If the object is a result
from the intamap procedure spatialPredict, the remaining arguments are not necessary.}
\item{formulaString}{formula that defines the dependent variable as a linear model of independent variables; 
  suppose the dependent variable has name z, for ordinary and simple kriging use the formula z~1; 
  for universal kriging, suppose z is linearly dependent on x and y, use the formula z~x+y}
\item{observations}{a \code{\link[sp:Spatial-class]{Spatial}}*DataFrame with observations; 
      should contain the dependent variable, independent variables, and coordinates}
\item{predictionLocations}{the predictionLocations, only necessary if the 
                           method is "IWQSEL" and formulaString contains independent variables.
                           Should preferentally be a grid if the method is "IWQSEL"}
\item{model}{variogram model of dependent variable (or its residuals), defined 
              by a call to \code{\link[gstat]{vgm}} or \code{\link[automap]{autofitVariogram}} }
\item{outputWhat}{Argument with type of unbiasedness method ("MOK" or "IWQSEL") and the thresholds.}
\item{yamamoto}{logical describing if the yamamoto approach )is to be used in simulations.
Defaults to yamamoto = FALSE when object is a \code{\link[sp:Spatial-class]{Spatial}}*DataFrame.}
\item{iwqmaxit}{maximum number of iterations in iwqsel}
\item{iwqCpAddLim}{convergence criteria in iwqsel}
\item{debug.level}{debug level, passed to subfunctions}
\item{...}{ other arguments that will be passed to subfunctions. These include 
\describe{
\item{\code{nsim}}{number of simulations necessary if the method is "IWQSEL".
Defaults to nsim = 100 when object is a \code{\link[sp:Spatial-class]{Spatial}}*DataFrame.}
\item{\code{maxdist}}{maximum number of neighbours to use in local kriging, defaults to Inf}
\item{\code{nmax}}{for local kriging: the number of nearest observations that 
should be used in simulations for the "IWQSEL" method in terms of the space of the spatial locations. 
Defaults to nmax = 10 when object is a Spatial*DataFrame.}
}
}
} 

\details{
It is a fact that predictions from kriging tend to be biased towards the mean of
the process. The function \code{unbiasedKrige} is a function that adds one or more predictions
to the original output, which are assumed to be unbiased relative to a certain 
threshold. The two methods supported are the IWQSEL-method (Craigmile, 2006) and
MOK (Skoien et al, 2008). 
}

\value{ 
an object of type intamap, as described in \code{\link{intamap-package}}, or a 
\code{\link[sp:Spatial-class]{Spatial}}*DataFrame with one or more new prediction columns, representing different
methods and thresholds.
}

\references{ 
Craigmile, P. F., N. Cressie, T. J. Santner, and Y. Rao. 2006. 
A loss function approach to identifying environmental exceedances. Extremes, 8, 143-159.

Skoien, J. O., G. B. M. Heuvelink, and E. J. Pebesma. 2008. 
Unbiased block predictions and exceedance probabilities for environmental thresholds. 
In: J. Ortiz C. and X. Emery (eds). Proceedings of the eight international geostatistics congress. 
Gecamin, Santiago, Chile,  pp. 831-840.

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}
\author{ Jon Olav Skoien}

\examples{
library(automap)
library(gstat)
data(meuse)
data(meuse.grid)
coordinates(meuse) = ~x+y
gridded(meuse.grid) = ~x+y

predictionLocations = meuse.grid[sample(1:length(meuse.grid),50),]
vmod = autofitVariogram(log(zinc)~1,meuse)$var_model
prediction = krige(log(zinc)~1,meuse,predictionLocations,vmod)
summary(prediction)

prediction <- unbiasedKrige(prediction,log(zinc)~1,
  meuse,  model = vmod, outputWhat = list(MOK = 6.0, MOK = 7.0, IWQSEL=7.0), 
  iwqmaxit = 100, iwqCpAddLim = 0.01)
summary(prediction)
}
\keyword{spatial}
