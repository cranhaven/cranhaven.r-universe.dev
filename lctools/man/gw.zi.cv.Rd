\name{gw.zi.cv}
\alias{gw.zi.cv}
\title{A specific version of the function gw.zi}
\description{A specific version of the function \code{\link{gw.zi}} returning only the leave-one-out Cross Validation (CV) score. \code{\link{gw.zi.cv}} exludes the observation for which a sub-model fits.}
\usage{
gw.zi.cv(bw, formula, family, dframe, obs, kernel, dmatrix, ...)
}
\arguments{
  \item{bw}{a positive number that may be an integer in the case of an "adaptive kernel" or a real in the case of a "fixed kernel". In the first case the integer denotes the number of nearest neighbours, whereas in the latter case the real number refers to the bandwidth (in meters if the coordinates provided are Cartesian). This argument can be also the result of a bandwidth selection algorithm such as those available in the function \code{gw.zi.bw}}
  \item{formula}{the local model to be fitted using the same syntax used in the zeroinfl function of the R package \code{pscl}. This is a sting (a symbolic description of the model) that is passed to the sub-models' \code{\link{zeroinfl}} function. For more details look at the details of the \code{\link{zeroinfl}} function.}
  \item{family}{a specification of the count model family to be used in the local model as in the \code{\link{zeroinfl}} function. Currently the only option tested is "poisson".}
  \item{dframe}{a numeric data frame of at least two suitable variables (one dependent and one independent)}
  \item{obs}{number of observations in the global dataset}
  \item{kernel}{the kernel to be used in the regression. Options are "adaptive" or "fixed". The weighting scheme used here is defined by the bi-square function \code{(weight = (1-(ndist/H)^2)^2} for distances less than or equal to \code{H}, \code{0} otherwise)}
  \item{dmatrix}{eucledian distance matrix between the observations}
  \item{...}{more arguments for the \code{\link{zeroinfl}} function}
}
\details{Only used by gw.zi.bw}

\value{Leave-one-out Cross Validation (CV) score}

\references{
Kalogirou, S. (2016) Destination Choice of Athenians: an application of geographically weighted versions of standard and zero inflated Poisson spatial interaction models, Geographical Analysis, 48(2),pp. 191-230. DOI: 10.1111/gean.12092 \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12092}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\seealso{
\code{\link{gw.zi.bw}}
}
\keyword{GWZIPR}
\keyword{local regression}