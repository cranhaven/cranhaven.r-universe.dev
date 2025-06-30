\name{summary}
\alias{summary}
\alias{summary.fusionModel}
\title{Obtain summary of parameter estimates for a spatial fusion model}
\usage{
\method{summary}{fusionModel}(object, digits = 3, ...)
}
\arguments{

\item{object}{object of class \code{fusionModel}. Output of \code{\link{fusion}()}.}

\item{digits}{integer. The number of significant digits.}

\item{...}{additional arguments not used.}
}

\value{
The returned value is a matrix containing the parameter estimates and their summary statistics. The names of fixed effect coefficients are covariate names followed by internal parameter names in parentheses. 'beta_p' denotes the coefficients for point data and 'beta_a' denotes the coefficients for lattice data.
}

\description{
Generate summary statistics for posterior parameter estimates from a spatial fusion model.
}

\author{
Craig Wang
}

\examples{
\dontshow{Sys.setenv("OMP_THREAD_LIMIT" = 2)}
## example based on simulated data
\donttest{
if (requireNamespace("INLA", quietly = TRUE)) {
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
                pp.data = dat$data$lgcp.coords[[1]],
                distributions = c("normal","poisson"), method = "INLA")

mod_inla <- fusion(data = dat_inla, n.latent = 1, bans = 0,
                prior.range = c(1, 0.5), prior.sigma = c(1, 0.5),
                mesh.locs = dat_inla$locs_point, mesh.max.edge = c(0.5, 1))

summary(mod_inla)
}
}
}
\keyword{methods}

