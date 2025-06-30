\name{fusion.dinla}
\alias{fusion.dinla}
\title{Fit a spatial fusion model using INLA}
\usage{
\method{fusion}{dinla}(data, n.latent = 1, bans = 0, pp.offset,
    verbose = FALSE, alpha = 3/2, prior.range,
    prior.sigma, prior.args, mesh.locs, mesh.max.edge,
    mesh.args, inla.args, ...)
}
\arguments{
\item{data}{an object of class \code{dinla}. Output of \code{\link{fusionData}()}.}

\item{n.latent}{integer. Number of latent processes to be modeled.}

\item{bans}{either 0 or a matrix of 0s and 1s with dimension J times n.latent, where J is the total number of response variables. If \code{matrix}, 1 indicates banning an association between the latent process and response variable. If 0, no association is banned.}

\item{pp.offset}{numeric, vector of numeric or matrix of numeric. Offset term for point pattern data.}

\item{verbose}{logical. If TRUE, prints progress and debugging information}

\item{alpha}{numeric between 0 and 2. Determines the covariance model, defined as \eqn{\nu + 1} for two dimensional space. Default value is 3/2 which corresponds to the exponential covariance model. See details.}

\item{prior.range}{vector of length 2, with (range0, Prange) specifying that P(\eqn{\rho\sqrt{8\nu}} < range0) = Prange, where \eqn{\rho\sqrt{8\nu}} is the practical spatial range of the random field. If Prange is NA, then range0 is used as a fixed range value. See details.}

\item{prior.sigma}{vector of length 2, with (sigma0, Psigma) specifying that P(\eqn{\sigma} > sigma0) = Psigma, where \eqn{\sigma} is the marginal standard deviation of the field. If Psigma is NA, then sigma0 is used as a fixed sigma value. See details.}

\item{prior.args}{named list. Other prior arguments for \code{inla.spde2.matern()} in \pkg{INLA}.}

\item{mesh.locs}{matrix with two columns, or a \code{SpatialPoints}, \code{SpatialPointsDataFrame} object. Locations to be used as initial triangulation nodes.}

\item{mesh.max.edge}{vector of length one or two. The largest allowed triangle edge length for inner (and optional outer extension) mesh.}

\item{mesh.args}{named list. Other mesh arguments passed to \code{inla.mesh.2d()} in \pkg{INLA}.}

\item{inla.args}{named list. Other inla arguments passed to \code{inla()} \pkg{INLA}.}

\item{...}{additional arguments not used}
}

\value{
The returned value is a list consists of

\item{model}{an object of class \code{inla} representing the fitted INLA model}
\item{mesh}{an object of class \code{inla.mesh} containing the mesh used.}
\item{data}{the data structure used to fit the model}
}

\description{
Fit a spatial fusion model using INLA based on the unifying framework proposed by Wang and Furrer (2021). One or more latent Gaussian process(es) is assumed to be associated with the spatial response variables.
}

\details{
The prior used for modeling the latent spatial processes is \code{inla.spde2.matern}. Each spatial component is named as \code{sij}, where \code{i} denotes the ith latent process and \code{j} denotes the jth variable. For example, \code{s12} is the first latent process that is associated with the second variable. The first variable (with the following ordering: geostatistical, lattice, point pattern data) that a spatial component is associated with will have the original component, then the subsequent spatial components associated with other variables are treated as 'copies' of the original component modified by a coefficient \code{Beta}, as one of the latent parameters.

The INLA approximation only works for Matern covariance function, which can be written as

\eqn{C(d) = \sigma^2/(2^{\nu-1}\Gamma(\nu)) * (d\sqrt{2\nu}/\rho)^\nu K_\nu (d\sqrt{2\nu}/\rho),}

where \eqn{d} is the Euclidean distance, \eqn{K_\nu} is a modified Bessel function, \eqn{\rho} is the spatial range, \eqn{\sigma^2} is the partial sill and \eqn{\nu} is the smoothness parameter. NOTE: the range parameter in INLA output is defined as \dQuote{practical range} as \eqn{\rho\sqrt{8\nu}}.
}

\references{
Wang, C., Furrer, R. and for the SNC Study Group (2021). Combining heterogeneous spatial datasets with process-based spatial fusion models: A unifying framework. \emph{Computational Statistics & Data Analysis}, 161, 107240.
}

\author{
Craig Wang
}

\seealso{\code{\link{fusionData}} for preparing data, \code{\link{fitted}} for extracting fitted values, \code{\link{predict}} for prediction.}

\examples{
## example based on simulated data
\donttest{
if (requireNamespace("INLA", quietly = TRUE)) {
\dontshow{Sys.setenv("OMP_THREAD_LIMIT" = 2)}
dat <- fusionSimulate(n.point = 50, n.area = 20, n.grid = 4,
               psill = 1, phi = 1, nugget = 0, tau.sq = 0.5,
               point.beta = list(rbind(1,5)),
               area.beta = list(rbind(-1, 0.5)),
               distributions = c("normal","poisson"),
               design.mat = matrix(c(1,1,1)))

geo_data <- data.frame(x = dat$mrf[dat$sample.ind, "x"],
                y = dat$mrf[dat$sample.ind, "y"],
                cov.point = dat$data$X_point[,2],
                outcome = dat$data$Y_point[[1]])

lattice_data <- cbind(dat$poly,
                     data.frame(outcome = dat$data$Y_area[[1]],
                     cov.area = dat$data$X_area[,2]))

dat_inla <- fusionData(geo.data = geo_data, geo.formula = outcome ~ cov.point,
                lattice.data = lattice_data, lattice.formula = outcome ~ cov.area,
                pp.data = dat$data$lgcp.coords[[1]], distributions = c("normal","poisson"),
                method = "INLA")

mod_inla <- fusion(data = dat_inla, n.latent = 1, bans = 0,
                prior.range = c(1, 0.5), prior.sigma = c(1, 0.5),
                mesh.locs = dat_inla$locs_point, mesh.max.edge = c(0.5, 1))

summary(mod_inla)
}
}
}

\keyword{models}

