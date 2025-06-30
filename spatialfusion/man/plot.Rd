\name{plot}
\alias{plot}
\alias{plot.fusionModel}
\title{Generate diagnostics plot for a fusion model}
\usage{
\method{plot}{fusionModel}(x, posterior = TRUE, interactive = TRUE, ...)
}
\arguments{

\item{x}{object of class \code{fusionModel}. Output of \code{\link{fusion}()}.}

\item{posterior}{logical. If \code{TRUE}, then shows posterior versus prior distributions of fixed effect coefficients and latent parameters.}

\item{interactive}{logical. If \code{TRUE}, then print messages in the terminal to proceed to next plots.}

\item{...}{additional arguments not used}
}

\value{No return value, called for side effects}

\description{
Plot model diagnostics for \code{fusionModel} objects.  By default, it shows posterior versus prior distributions of fixed effect coefficients and latent parameters. The names of fixed effect coefficients are covariate names followed by internal parameter names in parentheses. 'beta_p' denotes the coefficients for point data and 'beta_a' denotes the coefficients for lattice data.
}

\details{
When \code{posterior = FALSE}, then traceplot of posterior samples for the fixed effect coefficients and latent parameters are shown for Stan approach and the mesh overlayed with spatial data is shown for INLA approach.
}

\author{
Craig Wang
}

\examples{
## example based on simulated data
\donttest{
if (requireNamespace("INLA", quietly = TRUE)) {
\dontshow{Sys.setenv("OMP_THREAD_LIMIT" = 2)}
dat <- fusionSimulate(n.point = 20, n.area = 10, n.grid = 2,
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

plot(mod_inla, interactive = FALSE)
}
}
}
\keyword{aplot}

