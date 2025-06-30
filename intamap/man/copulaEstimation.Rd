\name{copulaEstimation}
\alias{copulaEstimation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ML-estimation of the spatial copula model parameters}
\description{
Estimates parameters of the spatial copula model using maximum likelihood.
}
\usage{
copulaEstimation(obj,margin,trend,correlation,anisotropy,copula,tol=0.001,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{Intamap object, see description in \code{\link{intamap-package}}}
  \item{margin}{list with the following elements:
  \describe{
    \item{\code{params}}{Starting values for the parameters of the marginal distribution (excluding trend parameters)}
    \item{\code{lower}}{Lower bounds for the values of the parameters of the marginal distribution (excluding trend parameters)}
    \item{\code{upper}}{Upper bounds for the values of the parameters of the marginal distribution (excluding trend parameters)}
    \item{\code{name}}{Name of the family of marginal distributions. Possible names are: "norm","lnorm","gev","t" and "logis"}
  }
  }
  \item{trend}{list with the following elements:
  \describe{
    \item{\code{params}}{Starting values for the parameters of the trend model (location parameter of the marginal distribution)}
    \item{\code{lower}}{Lower bounds for the values of the parameters of the trend model}
    \item{\code{upper}}{Upper bounds for the values of the parameters of the trend model}
    \item{\code{F}}{Design matrix.}
  }
  }
  \item{correlation}{list with the following elements:
  \describe{
    \item{\code{model}}{Correlation function model. Possible models are: "Ste", "Sph", "Gau" and "Exp"}
    \item{\code{params}}{Starting values for the parameters of the correlation function model}
    \item{\code{lower}}{Lower bounds for the values of the parameters of the correlation function model}
    \item{\code{upper}}{Upper bounds for the values of the parameters of the correlation function model}
  }
  }
  \item{anisotropy}{list with the following elements:
  \describe{
    \item{\code{params}}{Starting values for the parameters of geometric anisotropy. If \code{NULL}, then no anisotropy is considered.}
    \item{\code{lower}}{Lower bounds for the values of the parameters of geometric anisotropy. Usually \code{c(0,1)}}
    \item{\code{upper}}{Upper bounds for the values of the parameters of geometric anisotropy. Usually \code{c(pi,Inf)}}
  }
  }
  \item{copula}{list with the following elements:
  \describe{
    \item{\code{method}}{Either "norm" or "chisq", depending on which spatial copula model is used, the Gaussian or the chi-squared copula.}
    \item{\code{params}}{Only used in case of the chi-squared copula: the squared non-centrality parameter of the non-central chi-squared distribution. Controls how far the chi-squared copula is from the Gaussian copula.}
    \item{\code{lower}}{Only used in case of the chi-squared copula: the lower bound for the copula parameter. Usually set to \code{0}}
    \item{\code{upper}}{Only used in case of the chi-squared copula: the upper bound for the copula parameter. Usually set to \code{Inf}}
  }
  }
  \item{tol}{Tolerance level for the optimization process.}
  \item{...}{Arguments to be passed to \code{\link[stats:optim]{optim}}.}
}
\details{
\code{copulaEstimation} performs maximum likelihood estimation of all possible parameters included in the Gaussian and
 chi-squared spatial copula model: parameters of the predefined family of marginal distributions (including spatial trend 
 or external drift), correlation function parameters, parameters for geometric anisotropy and parameters for the copula 
 (only used for the chi-squared copula model). Due to the large number of variables that need to be optimized, a 
 profile-likelihood approach is used. Although convergence to a global optimum is not assured, the profile-likelihood method
  makes it less likely that the optimization routine, \code{\link[stats:optim]{optim}}, gets stuck in a local optimum. The result of 
  \code{copulaEstimation} is a list containing all parameter point estimates that are needed for plug-in spatial 
  prediction. It is advisable to check the output of the algorithm by trying different starting values for the optimization. 
}
\value{
  A list with the following elements:
  \item{margin}{Same as the input except that the list element "params" now consists of the optimized parameters of the marginal distribution function.}
  \item{trend}{Same as the input except that the list element "params" now consists of the optimized parameters of the trend model.}
  \item{correlation}{Same as the input except that the list element "params" now consists of the optimized parameters of the correlation function model.}
  \item{anisotropy}{Same as the input except that the list element "params" now consists of the optimized parameters of geometric anisotropy.}
  \item{copula}{Same as the input except that the list element "params" now consists of the optimized copula parameters.}
}
\references{
[1] Kazianka, H. and Pilz, J. (2009), Spatial Interpolation Using Copula-Based Geostatistical Models. GeoENV2008 - Geostatistics for Environmental Application (P. Atkinson, C. Lloyd, eds.), Springer, New York 


[2] Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 

}

\author{Hannes Kazianka}
\seealso{\code{\link{bayesCopula}}, \code{\link{spatialPredict}}, \code{\link{estimateParameters}}}
\examples{
data(intamapExampleObject)
## estimate parameters for the copula model

\dontrun{copula<-list(method="norm")
anisotropy <- list(lower = c(0, 1), upper = c(pi, Inf), params = c(pi/3, 2))
correlation <- list(model = "Ste", lower = c(0.01, 0.01, 0.01), upper = c(0.99, Inf, 20), 
                    params = c(0.05, 4, 3))
margin <- list(name = "gev", lower = c(0.01, -Inf), upper = c(Inf, Inf), params = c(30, 0.5))
trend <- list(F = as.matrix(rep(1, 196)), lower = -Inf, upper = Inf, params = 40)
estimates <- copulaEstimation(intamapExampleObject, margin, trend, correlation, anisotropy, copula)
## make predictions at unobserved locations
predictions <- bayescopula(intamapExampleObject, estimates, search = 25,
               calc = list(mean = TRUE, variance = TRUE, excprob = 40, quantile = 0.95))}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

