\name{bayesCopula}
\alias{bayesCopula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs spatial interpolation using copulas}
\description{
Calculates predictive mean, predictive variance, predictive quantiles and exceedance probabilities for certain thresholds in the spatial copula model.
}
\usage{
bayesCopula(obj,estimates,search=10,calc=list(mean=TRUE,variance=TRUE),testMean=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{Intamap object including observations and predictionLocations, see 
              \code{\link{intamap-package}}}
  \item{estimates}{List of estimated parameters (typically obtained by calling \code{\link{copulaEstimation}})}
  \item{search}{local prediction: number of observed locations considered for prediction at each unknown point}
  \item{calc}{list of what prediction type is required:
  \describe{ 
    \item{\code{mean = TRUE}}{\code{TRUE} if the predictive mean should be calculated, \code{FALSE} otherwise}
    \item{\code{variance = TRUE}}{\code{TRUE} if the predictive variance should be calculated, \code{FALSE} otherwise}
    \item{\code{quantiles = NULL}}{Vector of desired predictive quantiles, e.g. 0.95 or 0.05}
    \item{\code{excprob = NULL}}{Vector of thresholds, where the probability of exceeding this threshold is desired}
  }}
 \item{testMean}{Whether or not the predictive means (if calculated) should be tested for being reasonable.}
}
\details{
\code{bayesCopula} is used for plug-in prediction at unobserved spatial locations. The name of the function is somewhat 
misleading since  no Bayesian approach is implemented so far. It is possible to calculate numerically the predictive mean
 and variance for both the Gaussian and the chi-square spatial copula model. Exceedance probabilities and predictive 
 quantiles are only supported for the Gaussian copula model. Note that it may occur that the predictive distribution has 
 no finite moments. In this case, a possible predictor is the median of the predictive distribution. If \code{testMean=TRUE} and
the predictive means have no reasonable values, the median is automatically calculated and a warning is produced.

The copula prediction method is computationally demanding.
There is a possibility of running it as a parallel process by setting the parameter
\code{nclus > 1} for the interpolation process. This requires a previous installation
of the package \code{doParallel}.
}
\value{
  List with the following elements:
  \describe{
  \item{mean}{Mean of the predictive distribution. NULL if not calculated.}
  \item{variance}{Variance of the predtictive distribution. NULL if not calculated.}
  \item{quantiles}{Quantiles of the predictive distribution NULL if not calculated.}
  \item{excprob}{Probabilities for the predictive distribution to exceed predefined thresholds. NULL if not calculated.}
  }
}
\references{
[1] Kazianka, H. and Pilz, J. (2009), Spatial Interpolation Using Copula-Based Geostatistical Models. GeoENV2008 - Geostatistics for Environmental Application (P. Atkinson, C. Lloyd, eds.), Springer, New York 

[2] Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\author{Hannes Kazianka}
\seealso{\code{\link{copulaEstimation}}, \code{\link{spatialPredict}}, \code{\link{estimateParameters}}}
\examples{
\dontrun{
data(intamapExampleObject)
## estimate parameters for the copula model
copula <- list(method="norm")
anisotropy <- list(lower = c(0,1), upper = c(pi, Inf), params = c(pi/3, 2))
correlation <- list(model = "Ste", lower=c(0.01, 0.01, 0.01), upper = c(0.99, Inf, 20),
                    params = c(0.05, 4, 3))
margin <- list(name = "gev", lower = c(0.01, -Inf), upper = c(Inf, Inf), params = c(30, 0.5))
trend <- list(F = as.matrix(rep(1, 196)), lower = -Inf, upper = Inf, params = 40)
estimates <- copulaEstimation(intamapExampleObject, margin, trend, correlation, anisotropy, copula)
## make predictions at unobserved locations
predictions<-bayesCopula(intamapExampleObject, estimates, search = 25,
    calc = list(mean = TRUE, variance = TRUE, excprob = 40, quantile = 0.95))
}
}
\keyword{spatial}

