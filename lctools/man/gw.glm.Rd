\name{gw.glm}
\alias{gw.glm}
\title{Generalised Geographically Weighted Regression (GGWR)}
\description{This function allows for the calibration of a local model using a Generalised Geographically Weighted Regression (GGWR). At the moment this function has been coded in order to fit a Geographically Weighted Poisson Regression (GWPR) model.}
\usage{gw.glm(formula, family, dframe, bw, kernel, coords)}
\arguments{
  \item{formula}{the local model to be fitted using the same syntax used in the glm function in R. This is a sting that is passed to the sub-models' \code{\link{glm}} function. For more details look at the class \code{\link{formula}}.}
  \item{family}{a description of the error distribution and link function to be used in the local model as in the \code{\link{glm}} function. Currently the only option tested is "poisson".}
  \item{dframe}{a numeric data frame of at least two suitable variables (one dependent and one independent)}
  \item{bw}{a positive number that may be an integer in the case of an "adaptive kernel" or a real in the case of a "fixed kernel". In the first case the integer denotes the number of nearest neighbours, whereas in the latter case the real number refers to the bandwidth (in meters if the coordinates provided are Cartesian). This argument can be also the result of a bandwidth selection algorithm such as those available in the function \code{gw.glm.bw}}
  \item{kernel}{the kernel to be used in the regression. Options are "adaptive" or "fixed". The weighting scheme used here is defined by the bi-square function \eqn{(weight = (1-(ndist/H)^2)^2} for distances less than or equal to \eqn{H}, \eqn{0} otherwise)}
  \item{coords}{a numeric matrix or data frame of two columns giving the X,Y coordinates of the observations}
}
\details{
The Generalised Geographically Weighted Regression is a method recently proposed building on the simple GWR. It allows for the investigation of the existence of spatial non-stationarity in the relationship between a dependent and a set of independent variables in the cases in which the dependent function does not follow a normal distribution. This is possible by fitting a sub-model for each observation is space, taking into account the neighbour observations weighted by distance. A detailed description of the Geographically Weighted Poisson Regression currently supported here along with examples from internal migration modelling can be found in two publication by Kalogirou (2003, 2015). The difference of this functions to existing ones is that each time the sub-dataset is selected and the sub-model is fitted using R's \code{\link{glm}} function instead of fitting the complete local model with matrix algebra. The latter approach may be faster but more prone to rounding error and code crashing.
}
\value{
  \item{GGLM_LEst}{a numeric data frame with the local intercepts and the local parameter estimates for each independent variable in the model's formula.}
  \item{GGLM_LPvalues}{a numeric data frame with the local p-value for the local intercepts and the local parameter estimates for each independent variable in the model's formula.}
  \item{GGLM_GofFit}{a numeric data frame with residuals and local goodness of fit statistics (AIC, Deviance)}
}

\references{
Kalogirou, S. (2003) The Statistical Analysis and Modelling of Internal Migration Flows within England and Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}

Kalogirou, S. (2016) Destination Choice of Athenians: an application of geographically weighted versions of standard and zero inflated Poisson spatial interaction models, Geographical Analysis, 48(2),pp. 191-230. DOI: 10.1111/gean.12092 \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12092}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\note{
This function is under development. There should be improvements in future versions of the package lctools. Any suggestion is welcome!
}

\section{Warning}{Large datasets may take long to calibrate.}

\seealso{
\code{\link{gw.glm.bw}}
\code{\link{gw.zi}}
\code{\link{gwr}}
}
\examples{
RDF <- random.test.data(12,12,3,"poisson")
gwpr <- gw.glm(dep ~ X1 + X2, "poisson", RDF, 50, kernel = 'adaptive', cbind(RDF$X,RDF$Y))
}
\keyword{GWPR}
\keyword{local regression}