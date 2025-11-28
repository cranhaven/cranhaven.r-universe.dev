\name{gw.zi}
\alias{gw.zi}
\title{Geographically Weighted Zero Inflated Poisson Regression (GWZIPR)}
\description{This function allows for the calibration of a local model using the Geographically Weighted Zero Inflated Poisson Regression (GWZIPR).}
\usage{
gw.zi(formula, family, dframe, bw, kernel, coords, ...)
}
\arguments{
  \item{formula}{the local model to be fitted using the same syntax used in the zeroinfl function of the R package \code{pscl}. This is a sting (a symbolic description of the model) that is passed to the sub-models' \link{zeroinfl} function. For more details look at the details of the \code{\link{zeroinfl}} function.}
  \item{family}{a specification of the count model family to be used in the local model as in the \code{\link{zeroinfl}} function. Currently the only option tested is "poisson".}
  \item{dframe}{a numeric data frame of at least two suitable variables (one dependent and one independent)}
  \item{bw}{a positive number that may be an integer in the case of an "adaptive kernel" or a real in the case of a "fixed kernel". In the first case the integer denotes the number of nearest neighbours, whereas in the latter case the real number refers to the bandwidth (in meters if the coordinates provided are Cartesian). This argument can be also the result of a bandwidth selection algorithm such as those available in the function \code{gw.zi.bw}}
  \item{kernel}{the kernel to be used in the regression. Options are "adaptive" or "fixed". The weighting scheme used here is defined by the bi-square function \code{(weight = (1-(ndist/H)^2)^2} for distances less than or equal to \code{H}, \code{0} otherwise)}
  \item{coords}{a numeric matrix or data frame of two columns giving the X,Y coordinates of the observations}
  \item{...}{more arguments for the \code{\link{zeroinfl}} function}
}
\details{
The Geographically Weighted Zero Inflated Poisson Regression (GWZIPR) is a method recently proposed by Kalogirou(2015). It can be used with count data that follow a Poisson distribution and contain many zero values. The GWZIPR allows for the investigation of the existence of spatial non-stationarity in the relationship between a dependent and a set of independent variables while accounting for excess zeros. This is possible by fitting two seperate sub-models for each observation is space, taking into account the neighbour observations weighted by distance. The first submodel (count) models the non-zero values of the dependent variable while the second submodel (zero) models the zero values of the dependent variable. A detailed description of the GWZIPR along with examples from internal migration modelling is presented in the paper mentioned above (Kalogirou, 2015).}

\value{
  \item{ZI_LEst_count}{a numeric data frame with the local intercepts and the local parameter estimates for each independent variable in the model's formula for the count part of the Zero Inflated model.}
  \item{ZI_LEst_zero}{a numeric data frame with the local intercepts and the local parameter estimates for each independent variable in the model's formula for the zero part of the Zero Inflated model.}
  \item{ZI_LPvalues_count}{a numeric data frame with the local p-value for the local intercepts and the local parameter estimates for each independent variable in the model's formula for the count part of the Zero Inflated model.}
  \item{ZI_LPvalues_zero}{a numeric data frame with the local p-value for the local intercepts and the local parameter estimates for each independent variable in the model's formula for the zero part of the Zero Inflated model.}
  \item{ZI_GofFit}{a numeric data frame with residuals and local goodness of fit statistics (AIC)}
}

\references{
Kalogirou, S. (2016) Destination Choice of Athenians: an application of geographically weighted versions of standard and zero inflated Poisson spatial interaction models, Geographical Analysis, 48(2),pp. 191-230. DOI: 10.1111/gean.12092 \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12092}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\note{This function is under development. There should be improvements in future versions of the package lctools. Any suggestion is welcome!
}

\section{Warning}{Large datasets may take long to calibrate.}

\seealso{
\code{\link{gw.zi.bw}}
\code{\link{gw.glm}}
\code{\link{gwr}}
}
\examples{
RDF <- random.test.data(10,10,3,"zip")
gw.zip <- gw.zi(dep ~ X1 + X2, "poisson", RDF, 60, kernel = 'adaptive', cbind(RDF$X,RDF$Y))
}
\keyword{GWZIPR}
\keyword{local regression}