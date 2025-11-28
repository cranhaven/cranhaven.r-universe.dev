\name{gwr.cv}
\alias{gwr.cv}
\title{A specific version of the function gwr}
\description{A specific version of the function \code{\link{gwr}} returning only the leave-one-out Cross Validation (CV) score. \code{\link{gwr.cv}} exludes the observation for which a sub-model fits.
}
\usage{
gwr.cv(bw, formula, dframe, obs, kernel, dmatrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bw}{a positive number that may be an integer in the case of an "adaptive kernel" or a real in the case of a "fixed kernel". In the first case the integer denotes the number of nearest neighbours, whereas in the latter case the real number refers to the bandwidth (in meters if the coordinates provided are Cartesian). This argument can be also the result of a bandwidth selection algorithm such as those available in the function \code{gwr.bw}}
  \item{formula}{the local model to be fitted using the same syntax used in the lm function in R. This is a sting that is passed to the sub-models' \code{\link{lm}} function. For more details look at the class \code{\link{formula}}.}
  \item{dframe}{a numeric data frame of at least two suitable variables (one dependent and one independent)}
  \item{obs}{number of observations in the global dataset}
  \item{kernel}{the kernel to be used in the regression. Options are "adaptive" or "fixed". The weighting scheme used here is defined by the bi-square function \code{(weight = (1-(ndist/H)^2)^2} for distances less than or equal to \code{H}, \code{0} otherwise)}
  \item{dmatrix}{eucledian distance matrix between the observations}
}

\details{Only used by gwr.bw}

\value{Leave-one-out Cross Validation (CV) score}

\references{
Fotheringham, A.S., Brunsdon, C., Charlton, M. (2000). Geographically Weighted Regression: the analysis of spatially varying relationships. John Wiley and Sons, Chichester.

Kalogirou, S. (2003) The Statistical Analysis and Modelling of Internal Migration Flows within England and Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\seealso{
\code{\link{gwr.bw}}
\code{\link{gwr}}
}

\keyword{GWR}
\keyword{local regression}