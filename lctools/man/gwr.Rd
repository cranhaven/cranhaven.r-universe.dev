\name{gwr}
\alias{gwr}
\title{Geographically Weighted Regression (GWR)}
\description{
This function allows for the calibration of a local model using a simple Geographically Weighted Regression (GWR)
}
\usage{
gwr(formula, dframe, bw, kernel, coords)
}
\arguments{
  \item{formula}{the local model to be fitted using the same syntax used in the lm function in R. This is a sting that is passed to the sub-models' \code{\link{lm}} function. For more details look at the class \code{\link{formula}}.}
  \item{dframe}{a numeric data frame of at least two suitable variables (one dependent and one independent)}
  \item{bw}{a positive number that may be an integer in the case of an "adaptive kernel" or a real in the case of a "fixed kernel". In the first case the integer denotes the number of nearest neighbours, whereas in the latter case the real number refers to the bandwidth (in meters if the coordinates provided are Cartesian). This argument can be also the result of a bandwidth selection algorithm such as those available in the function \code{gwr.bw}}
  \item{kernel}{the kernel to be used in the regression. Options are "adaptive" or "fixed". The weighting scheme used here is defined by the bi-square function \code{(weight = (1-(ndist/H)^2)^2} for distances less than or equal to \code{H}, \code{0} otherwise)}
  \item{coords}{a numeric matrix or data frame of two columns giving the X,Y coordinates of the observations}
}
\details{
The Geographically Weighted Regression (GWR) is a method of local regression introduced in the late 1990s. It allows for the investigation of the existence of spatial non-stationarity in the relationship between a dependent and a set of independent variables. This is possible by fitting a sub-model for each observation is space, taking into account the neighbour observations weighted by distance. A detailed description of the GWR method along with examples from the real estate market can be found in the book by Fotheringham et al. (2000). An application of GWR in internal migration modelling has been presented by Kalogirou (2003). The difference of this functions to existing ones is that each time the sub-dataset is selected and the sub-model is fitted using R's \code{\link{lm}} function instead of fitting the complete GWR model with matrix algebra. The latter approach may be faster but more prone to rounding error and code crashing.
}
\value{
  \item{LM_LEst}{a numeric data frame with the local intercepts and the local parameter estimates for each independent variable in the model's formula.}
  \item{LM_LPvalues}{a numeric data frame with the local p-value for the local intercepts and the local parameter estimates for each independent variable in the model's formula.}
  \item{LM_GofFit}{a numeric data frame with residuals and local goodness of fit statistics (AIC, Deviance)}
  }

\references{
Fotheringham, A.S., Brunsdon, C., Charlton, M. (2000). Geographically Weighted Regression: the analysis of spatially varying relationships. John Wiley and Sons, Chichester.

Kalogirou, S. (2003) The Statistical Analysis and Modelling of Internal Migration Flows within England and Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\note{
This function is under development. There should be improvements in future versions of the package lctools. Any suggestion is welcome!
}

\section{Warning}{Large datasets may take long to calibrate.}

\seealso{
\code{\link{gwr.bw}}
\code{\link{gw.glm}}
\code{\link{gw.zi}}
}
\examples{
data(GR.Municipalities)
Coords<-cbind(GR.Municipalities@data$X, GR.Municipalities@data$Y)
local.model<-gwr(Income01 ~ UnemrT01, GR.Municipalities@data, 50, kernel = 'adaptive', Coords)
}

\keyword{GWR}
\keyword{local regression}