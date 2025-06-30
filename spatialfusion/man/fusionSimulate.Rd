\name{fusionSimulate}
\alias{fusionSimulate}
\title{Simulate spatial data}
\usage{
fusionSimulate(n.point, n.area, n.grid, n.pred, dimension = 10,
               psill = 5, phi = 1, nugget = 0, tau.sq = 1,
               domain = NULL, point.beta = NULL, area.beta = NULL,
               nvar.pp = 1, distributions,
               design.mat = matrix(c(1, 1.5, 2)),
               pp.offset, seed)
}
\arguments{
\item{n.point}{positive integer. Sample size for geostatistical (point) data.}

\item{n.area}{positive integer. Sample size for lattice (areal) data.}

\item{n.grid}{positive integer. Number of grid to be divided in each direction of the spatial domain.}

\item{n.pred}{positive integer. Number of prediction locations to sample regularly.}

\item{dimension}{positive integer. Dimension of the square spatial domain.}

\item{domain}{an object of class \code{sf}. The spatial domain considered for the simulation. If NULL, a square domain with length \code{dimension} is used.}

\item{psill}{positive numeric or numeric vector. Partial sill parameter(s) of the latent Gaussian process(es).}

\item{phi}{positive numeric or numeric vector. Range parameter(s) of the latent Gaussian process(es).}

\item{nugget}{positive numeric vector. Nugget parameter(s) of the latent Gaussian process(es).}

\item{tau.sq}{positive numeric vector. Variance component(s) for normally distributed responses.}

\item{point.beta}{a list of column matrices. Regression coefficient for geostatistical data, e.g. \code{list(rbind(1,2,3),rbind(2,4,6))} for two geostatistical response variables with an intercept plus two covariates each.}

\item{area.beta}{a list of column matrices. Regression coefficient for lattice data, e.g. see above.}

\item{nvar.pp}{numeric. Number of point pattern response variables to simulate.}

\item{distributions}{character vector. Names of distributions for each dependent variables with geostatistical and lattice type, currently \dQuote{Gaussian} or \dQuote{normal}, \dQuote{Poisson} (count) and \dQuote{Bernoulli} (binary) are supported. Note: no distribution for point pattern data is required to be specified. }

\item{design.mat}{matrix. Design matrix for the latent Gaussian process(es), with the number of columns equal to the number of latent processes.}

\item{pp.offset}{numeric. A single offset term for the intensity of point pattern data.}

\item{seed}{integer. Random seed.}
}
\value{
The returned value is a list that consists of:

\item{data}{a named list providing data variables.}

\item{mrf}{a data.frame of locations and the latent Gaussian process.}

\item{domain}{an object of class \code{sf}  describing the whole domain.}

\item{pred.loc}{a data.frame of locations for prediction.}

\item{pred.ind}{a vector of indices for prediction locations.}

\item{sample.ind}{a vector providing the indices of sampled locations in the Gaussian process. (only if there is geostatistical data)}

\item{mean.w}{a list of aggregated latent process for each area. (only if there is lattice data)}

\item{poly}{an object of class \code{sf} for the lattice data. (only if there is lattice data)}

\item{lgcp.grid}{a data.frame containing the centroids of gridded cells for point pattern data and the corresponding event counts. (only if there is point pattern data)}
}
\description{
Simulate spatial response variables with different data types, including geostatistical (point), lattice (areal), and point pattern data. They share common latent Gaussian processes. The geostatistical and lattice response variables are allowed to have fixed effects.
}
\details{
The exponential covariance model is used,

\eqn{C(d) = \sigma^2 \exp{-d/\phi}}

where \eqn{d} is the Euclidean distance, \eqn{\sigma^2} is the partial sill and \eqn{\phi} is the spatial range.

If the purpose is to validate a fitted latent spatial components of a spatial fusion model, one can check the fitted latent values against \code{mrf[sample.ind, -1:2]}. If the purpose is to investigate prediction performance of latent spatial components, one can predict at locations \code{pred.loc} and check against \code{mrf[pred.ind, -1:2]}.
}

\seealso{\code{\link{fusion}}, \code{\link{fusion.dstan}}}

\author{
Craig Wang
}

\examples{
\dontshow{Sys.setenv("OMP_THREAD_LIMIT" = 2)}
# three responses with a single latent Gaussian process
dat1 <- fusionSimulate(n.point = 50, n.area = 5, n.grid = 2,
            psill = 1, phi = 1, nugget = 0, tau.sq = 0.5,
            point.beta = list(rbind(1,5)), area.beta = list(rbind(-1, 0.5)),
            distributions = c("normal","poisson"), pp.offset = 1,
            design.mat = matrix(c(1,1,1)))

\donttest{
# three responses with two latent Gaussian processes
dat2 <- fusionSimulate(n.point = 50, n.area = 5, n.grid = 2,
               psill = c(1,2), phi = c(2,1), nugget = c(0,0), tau.sq = 1,
               point.beta = list(rbind(1,5)), area.beta = list(rbind(-1, 0.5)),
               distributions = c("normal","poisson"), pp.offset = 0.1,
               design.mat = matrix(c(1,1,1,2,3,4), ncol = 2))
}
}
\keyword{datagen}

