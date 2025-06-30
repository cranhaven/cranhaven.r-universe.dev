\name{fitted}
\alias{fitted}
\alias{fitted.fusionModel}
\title{Obtain fitted values of spatial fusion model}
\usage{
\method{fitted}{fusionModel}(object, type = c("link", "summary", "full", "latent"), ...)
}
\arguments{

\item{object}{object of class \code{fusionModel}. Output of \code{\link{fusion}()}.}

\item{type}{string. The default "link" gives the median of linear predictors; "summary" gives the mean, standard deviation and quantiles of linear predictors; "full" gives full marginals for INLA or posterior samples for Stan; "latent" gives the median of latent processes with their corresponding locations.}

\item{...}{additional arguments not used.}
}

\value{
The returned value is a list containing the fitted results for each response variable.
}

\description{
Generate fitted values of the response variables based on a spatial fusion model.
}

\details{
For INLA models, no posterior values for point pattern data will be generated.
}

\seealso{\code{\link{fusion}}, \code{\link{fusion.dinla}}, \code{\link{fusion.dstan}}.}

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

lattice_data <-cbind(dat$poly,
                     data.frame(outcome = dat$data$Y_area[[1]],
                    cov.area = dat$data$X_area[,2]))

dat_inla <- fusionData(geo.data = geo_data, geo.formula = outcome ~ cov.point,
                lattice.data = lattice_data, lattice.formula = outcome ~ cov.area,
                pp.data = dat$data$lgcp.coords[[1]],
                distributions = c("normal","poisson"),
                method = "INLA")

mod_inla <- fusion(data = dat_inla, n.latent = 1, bans = 0,
                prior.range = c(1, 0.5), prior.sigma = c(1, 0.5),
                mesh.locs = dat_inla$locs_point, mesh.max.edge = c(0.5, 1))

fit_inla <- fitted(mod_inla, type = "summary")
}
}
}
\keyword{methods}

