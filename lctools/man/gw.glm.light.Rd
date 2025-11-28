\name{gw.glm.light}
\alias{gw.glm.light}
\title{A light version of the Generalised Geographically Weighted Regression (GGWR)}
\description{This function allows for the calibration of a local model using the Generalised Geographically Weighted Regression (GGWR) but reports and returns fewer results compared to the function \code{\link{gw.glm}}.
}
\usage{
gw.glm.light(formula, family, dframe, bw, kernel, coords)
}
\arguments{
  \item{formula}{the local model to be fitted using the same syntax used in the glm function in R. This is a sting that is passed to the sub-models' \code{\link{glm}} function. For more details look at the class \code{\link{formula}}.}
  \item{family}{a description of the error distribution and link function to be used in the local model as in the \code{\link{glm}} function. Currently the only option tested is "poisson".}
  \item{dframe}{a numeric data frame of at least two suitable variables (one dependent and one independent)}
  \item{bw}{a positive number that may be an integer in the case of an "adaptive kernel" or a real in the case of a "fixed kernel". In the first case the integer denotes the number of nearest neighbours, whereas in the latter case the real number refers to the bandwidth (in meters if the coordinates provided are Cartesian). This argument can be also the result of a bandwidth selection algorithm such as those available in the function \code{gw.glm.bw}}
  \item{kernel}{the kernel to be used in the regression. Options are "adaptive" or "fixed". The weighting scheme used here is defined by the bi-square function \code{(weight = (1-(ndist/H)^2)^2} for distances less than or equal to \code{H}, \code{0} otherwise)}
  \item{coords}{a numeric matrix or data frame of two columns giving the X,Y coordinates of the observations}
}
\details{
For more details look at the function \code{\link{gw.glm}}. \code{\link{gw.glm.light}} is only used by the function \code{\link{gw.glm.mc.test}} in order to asses if the local parameter estimates of the Generalised Geographically Weighted Regression (GGWR) exhibit a significant spatial variation.}

\value{A numeric data frame with the local intercepts and the local parameter estimates for each independent variable in the model's formula.}

\references{
Kalogirou, S. (2016) Destination Choice of Athenians: an application of geographically weighted versions of standard and zero inflated Poisson spatial interaction models, Geographical Analysis, 48(2),pp. 191-230. DOI: 10.1111/gean.12092 \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12092}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\seealso{
\code{\link{gw.glm}}, 
\code{\link{gw.glm.mc.test}}
}

\keyword{GGWR}
\keyword{local regression}
