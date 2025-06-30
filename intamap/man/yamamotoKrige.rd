\name{yamamotoKrige}
\alias{yamamotoKrige}

\title{kriging and simulation with alternative kriging variance}

\description{ordinary kriging and simulation with an alternative kriging variance}

\usage{yamamotoKrige(formula, Obs, newPoints, model, nsim = 0, nmax = 20, maxdist = Inf)}
\arguments{
\item{formula}{formula that defines the dependent variable as a linear model of 
               independent variables; suppose the dependent variable has name 
               \code{z}, for ordinary and simple kriging use the formula \code{z~1}; 
               only ordinary kriging is currently implemented, \code{formula} is
               hence mainly used to identify the dependent variable }
\item{Obs}{\code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}} with observations}
\item{newPoints}{\code{\link[sp:Spatial-class]{Spatial}} object with prediction locations, either points or grid}
\item{model}{variogram model - of the type that can be found by a call to 
            \code{\link[gstat:vgm]{vgm}} }
\item{nsim}{integer; if set to a non-zero value, conditional simulation is used 
            instead of kriging interpolation. For this, sequential Gaussian simulation 
            is used, following a single random path through the data. }
\item{nmax}{for local kriging: the number of nearest observations that should 
            be used for a kriging prediction or simulation, where nearest is 
            defined in terms of the space of the spatial locations. 
            By default, all observations are used. }
\item{maxdist}{maximum number of neighbours to use in local kriging, defaults to Inf}
} 

\value{
Either a \code{\link[sp:Spatial-class]{Spatial}}*DataFrame with predictions and prediction variance, 
in the columns \code{var1.pred} and \code{var1.var}, together with the
classical ordinary kriging variance in \code{var1.ok}, or simulations with 
column names \code{simx} where x is the number of the simulation.
}




\details{

The term \code{yamamotoKrige} comes from the paper of Yamamoto (2000) where
he suggests using local variance around the kriging estimate (weighted with
the kriging weights) as an alternative kriging variance. This as a 
solution to more reliable estimates of the kriging variance also when the 
stationarity assumption has been violated. The method was applied by 
Skoien et al. (2008), who showed that it can have advantages for cases where the 
stationarity assumption behind kriging is violated.

If the number of observations is high, it is recommended have \code{nmax} lower.
This is partly because the method relies on positive kriging weights. The method
to do this adds the norm of the largest negative weight to all weights, and rescales.
This tends to smooth the weights, giving a prediction closer to the average if a 
too large number of observation locations is used.
}

\references{
Skoien, J. O., G. B. M. Heuvelink, and E. J. Pebesma. 2008. 
Unbiased block predictions and exceedance probabilities for environmental thresholds. 
In: J. Ortiz C. and X. Emery (eds). Proceedings of the eight international 
geostatistics congress. Santiago, Chile: Gecamin, pp. 831-840.

Yamamoto, J. K. 2000. An alternative measure of the reliability of ordinary 
kriging estimates. Mathematical Geology, 32 (4), 489-509.

Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\author{Jon Olav Skoien}
\examples{
library(gstat)
library(automap)
data(sic2004)
coordinates(sic.val) = ~x+y
coordinates(sic.test) = ~x+y
variogramModel = autofitVariogram(joker~1,sic.val)$var_model
newData = yamamotoKrige(joker~1,sic.val,sic.test,variogramModel,nmax = 20)
summary(newData)
plot(sqrt(var1.ok)~var1.pred,newData) 
# Kriging variance the same in regions with extreme values
plot(sqrt(var1.var)~var1.pred,newData) 
# Kriging standard deviation higher for high predictions (close to extreme values)
}


\keyword{spatial}
