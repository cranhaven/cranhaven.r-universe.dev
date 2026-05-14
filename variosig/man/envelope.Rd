\name{envelope}
\alias{envelope}
\alias{envelope.gstatVariogram}
\alias{envelope.variogram}
\title{Generate Pointwise Variogram Envelope}
\description{
Apply Monte Carlo permutation on the data values over different locations, and compute the variogram estimates for each permutation. The pointwise variogram envelope is taken based on quantiles of variogram estimates at each distance binning.
}
\usage{
\method{envelope}{gstatVariogram}(vario, data, locations = coordinates(data),
  formula = NULL, cluster = FALSE, n.cluster = NULL,
  nsim = 999, conf.level = 0.95, save.sim = FALSE, ...)

\method{envelope}{variogram}(vario, data, locations = data$coords,
  trend = NULL, cluster = FALSE, n.cluster = NULL,
  nsim = 999, conf.level = 0.95, save.sim = FALSE, ...)
}

\arguments{
\item{vario}{variogram of class \code{gstatVariogram} from the \code{gstat} package or \code{variogram} from the \code{geoR} package.}
\item{data}{an object of class \code{SpatialPointsDataFrame} if \code{vario} is from \code{geoR} or class \code{geodata} if \code{vario} is from \code{gstat}.}
\item{locations}{matrix. Contains x-coordinates in the first column and y-coordinates in the second column.}
\item{formula}{only works with \code{gstat} package, a formula defining the response variable and possible covariates.}
\item{trend}{only works with \code{geoR} package, definng the response variable and possible covariates. See documentation of \code{\link[geoR]{trend.spatial}} for further details.}
\item{cluster}{logical. If \code{TRUE}, clustering-based permutation is used. See Details.}
\item{n.cluster}{integer. The number of clusters used in the permutation. Only used when \code{cluster = TRUE}.}
\item{nsim}{integer. The number of permutations conducted to compute the envelope. The default is 999.}
\item{conf.level}{confidence level of the envelope. The default is 0.95.}
\item{save.sim}{logical. If \code{TRUE}, save all the permutated data.}
\item{...}{any other arguments that will be passed to variogram computation using either \code{\link[gstat]{variogram}} from \code{gstat} package or \code{\link[geoR]{variog}} from \code{geoR} package.}
}

\details{
There are two possible permutation methods implemented. If \code{cluster = FALSE}, complete spatial randomness is assumed and values are permuted over all locations. If \code{cluster = TRUE}, spatial dependence at small scales is assumed and and values are permuted only within clusters which are determined using kmeans clustering over spatial locations. When the interest is about the existence of spatial dependence at small scales, \code{cluster = TRUE} is preferred to acheive a higher statistical power.
}

\value{
A list with the following elements:
\item{data}{contains a list of all permutated data, only exists if \code{save.sim=TRUE}}
\item{variogram}{the original variogram estimates and estimates from each of the permutations}
\item{upper}{upper quantile of the variogram estimates according to the confidence level}
\item{lower}{lower quantile of the variogram estimates according to the confidence level}
\item{data.values}{the values used for variogram estimation, namely the residuals after removing covariates' effect}
\item{variogram0}{the original variogram supplied via \code{vario} argument}
}

\references{
Diggle, P.J., Ribeiro, P.J. (2007), Model-Based Geostatistics, Springer Series in Statistics. Springer.

Wang, C., Furrer, R. (2018) Monte Carlo Permutation Tests for Assessing Spatial Dependence at Difference Scales. Nonparametric Statistics. (Submitted)}

\author{Craig Wang, Reinhard Furrer}
\seealso{
\code{\link{envplot}} to plot an empirical variogram based on the data and its envelope
}
\examples{
\dontrun{
library(sp)
data(meuse)

## for gstat variogram

library(gstat)
data(meuse)
coordinates(meuse) = ~x+y
vario0 <- gstat::variogram(log(zinc)~1, meuse)
varioEnv <- envelope(vario0, data = meuse,
  formula = log(zinc)~1, nsim = 499, cluster = TRUE, n.cluster = 10)

## for geoR variog

library(geoR)
data(meuse)
meuse <- as.geodata(meuse, coords.col = 1:2, data.col = 6, covar.col = 3:5)
meuse$data <- log(meuse$data)
vario0 <- variog(meuse,max.dist=1500)
varioEnv <- envelope(vario0, data = meuse,
  trend = trend.spatial(~lead, meuse), nsim = 499)
}}
\keyword{models}
