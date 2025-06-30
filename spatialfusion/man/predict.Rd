\name{predict}
\alias{predict}
\alias{predict.fusionModel}
\title{Obtain predictions for the latent processes of spatial fusion model}
\usage{
\method{predict}{fusionModel}(object, new.locs, type = c("summary", "full"), ...)
}
\arguments{
\item{object}{an object of class \code{fusionModel}. Output of \code{\link{fusion}()}.}

\item{new.locs}{of class \code{data.frame} or \code{sf}. Contains the locations where the latent process(es) will be predicted. If \code{data.frame}, it must have column names "x" and "y" as coordinates of observations.}

\item{type}{string, The default "summary" gives posterior median of latent process(es); "full" gives full marginals (for INLA) or posterior samples (for Stan) of latent process(es).}

\item{...}{additional arguments not used}
}

\value{
The returned value is an object of class \code{sf} containing the posterior values for the latent spatial components and the prediction location in the geometry column of type \code{POINTS}.

For INLA models, the output represents the latent components that are associated with each response variable multiplied by the design matrix Z. They are indexed with \code{ij}, where \code{i} denotes the ith latent process and \code{j} denotes the jth variable. The variables are ordered by geostatistical, lattice, point pattern data.

For Stan models, the output represents the original latent components before multiplied by the deisng matrix Z. Each spatial component is indexed with \code{i}, where \code{i} denotes the ith latent process.
}

\description{
Generate posterior values containing predictions of the latent Gaussian process(es) based on a fitted spatial fusion model and new locations.
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
                pp.data = dat$data$lgcp.coords[[1]],
                distributions = c("normal","poisson"), method = "INLA")

mod_inla <- fusion(data = dat_inla, n.latent = 1, bans = 0,
            prior.range = c(1, 0.5), prior.sigma = c(1, 0.5),
            mesh.locs = dat_inla$locs_point, mesh.max.edge = c(0.5, 1))

pred_inla <- predict(mod_inla, dat$pred.loc, type = "summary")
}
}
}
\keyword{methods}

