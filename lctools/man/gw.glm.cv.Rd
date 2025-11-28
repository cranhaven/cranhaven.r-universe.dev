\name{gw.glm.cv}
\alias{gw.glm.cv}
\title{A specific version of the function gw.glm}
\description{A specific version of the function \code{\link{gw.glm}} returning only the leave-one-out Cross Validation (CV) score. \code{\link{gw.glm.cv}} exludes the observation for which a sub-model fits.}
\usage{
gw.glm.cv(bw, formula, family, dframe, obs, kernel, dmatrix)
}
\arguments{
  \item{bw}{a positive number that may be an integer in the case of an "adaptive kernel" or a real in the case of a "fixed kernel". In the first case the integer denotes the number of nearest neighbours, whereas in the latter case the real number refers to the bandwidth (in meters if the coordinates provided are Cartesian).}
  \item{formula}{the local model to be fitted using the same syntax used in the glm function in R. This is a sting that is passed to the sub-models' \code{\link{glm}} function. For more details look at the class \code{\link{formula}}.}
  \item{family}{a description of the error distribution and link function to be used in the local model as in the \code{\link{glm}} function. Currently the only option tested is "poisson".}
  \item{dframe}{a numeric data frame of at least two suitable variables (one dependent and one independent)}
  \item{obs}{number of observations in the global dataset}
  \item{kernel}{the kernel to be used in the regression. Options are "adaptive" or "fixed". The weighting scheme used here is defined by the bi-square function \code{(weight = (1-(ndist/H)^2)^2} for distances less than or equal to \code{H}, \code{0} otherwise)}
  \item{dmatrix}{eucledian distance matrix between the observations}
}
\details{Only used by gw.glm.bw}

\value{Leave-one-out Cross Validation (CV) score}

\references{
Kalogirou, S. (2003) The Statistical Analysis and Modelling of Internal Migration Flows within England and Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}

Kalogirou, S. (2016) Destination Choice of Athenians: an application of geographically weighted versions of standard and zero inflated Poisson spatial interaction models, Geographical Analysis, 48(2),pp. 191-230. DOI: 10.1111/gean.12092 \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12092}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\seealso{
\code{\link{gw.glm.bw}}
}
\keyword{GWPR}
\keyword{local regression}