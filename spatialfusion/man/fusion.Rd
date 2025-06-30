\name{fusion}
\alias{fusion}
\title{Fit a spatial fusion model}
\usage{

fusion(data, n.latent = 1, bans = 0, pp.offset,
       verbose = FALSE, ...)
}
\arguments{
\item{data}{an object of class either \code{dstan} or \code{dinla}. Output of \code{\link{fusionData}()}.}

\item{n.latent}{integer. Number of latent processes to be modeled.}

\item{bans}{either 0 or a matrix of 0s and 1s with dimension J times n.latent, where J is the total number of response variables. If \code{matrix}, 1 indicates banning an association between the latent process and response variable. If 0, no association is banned.}

\item{pp.offset}{numeric, vector of numeric or matrix of numeric. Offset term for point pattern data.}

\item{verbose}{logical. If TRUE, prints progress and debugging information.}

\item{...}{additional arguments depending on the class of \code{data}}
}

\value{
The returned value is a named list of class \code{fusionModel} consisting of model output and data structure used. If the model is fitted with INLA, the mesh used is also included.
}

\description{
Fit a spatial fusion model based on the unifying framework proposed by Wang and Furrer (2021). One or more latent Gaussian process(es) is assumed to be associated with the spatial response variables.
}

\details{
It is not possible to add covariates for point pattern data. However, an offset term can be supplied. Any covariate information can be taken into account by firstly fit a fixed effect model and enter the fitted values into the offset term as \code{pp.offset}.
}

\references{
Wang, C., Furrer, R. and for the SNC Study Group (2021). Combining heterogeneous spatial datasets with process-based spatial fusion models: A unifying framework. \emph{Computational Statistics & Data Analysis}, 161, 107240.
}

\author{
Craig Wang
}

\seealso{\code{\link{fusion.dinla}}, \code{\link{fusion.dstan}}, \code{\link{fusionData}} for preparing data, \code{\link{fitted.fusionModel}} for extracting fitted values, \code{\link{predict.fusionModel}} for prediction.}


\examples{
\dontshow{Sys.setenv("OMP_THREAD_LIMIT" = 2)}
## example based on simulated data
\donttest{
dat <- fusionSimulate(n.point = 20, n.area = 10,
          psill = 1, phi = 1, nugget = 0, tau.sq = 0.5,
          point.beta = list(rbind(1,5)),
          area.beta = list(rbind(-1, 0.5)),
          distributions = c("normal","poisson"),
          design.mat = matrix(c(1,1)))

geo_data <- data.frame(x = dat$mrf[dat$sample.ind, "x"],
                y = dat$mrf[dat$sample.ind, "y"],
                cov.point = dat$data$X_point[,2],
                outcome = dat$data$Y_point[[1]])

lattice_data <- cbind(dat$poly,
                     data.frame(outcome = dat$data$Y_area[[1]],
                     cov.area = dat$data$X_area[,2]))

dat_stan <- fusionData(geo.data = geo_data, geo.formula = outcome ~ cov.point,
                lattice.data = lattice_data, lattice.formula = outcome ~ cov.area,
                distributions = c("normal","poisson"),
                method = "Stan")

## S3 method for class 'dstan'
mod_stan <- fusion(data = dat_stan, n.latent = 1, bans = 0,
                prior.phi = list(distr = "normal", pars = c(1, 10)),
                nsamples = 1000, nburnin = 500, nchain = 1, ncore = 1)
summary(mod_stan)
}

\donttest{
if (requireNamespace("INLA", quietly = TRUE)) {
dat <- fusionSimulate(n.point = 20, n.area = 10, n.grid = 10, n.pred =15,
          psill = 1, phi = 1, nugget = 0, tau.sq = 0.5,
          point.beta = list(rbind(1,5)),
          area.beta = list(rbind(-1, 0.5)), nvar.pp = 1,
          distributions = c("normal","poisson"),
          design.mat = matrix(c(1,1,1), ncol=1), pp.offset = 1)

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


## S3 method for class 'dinla'
mod_inla <- fusion(data = dat_inla, n.latent = 1, bans = 0,
                prior.range = c(1, 0.5), prior.sigma = c(1, 0.5),
                mesh.locs = dat_inla$locs_point, mesh.max.edge = c(0.5, 1))
summary(mod_inla)
}
}
}
\keyword{models}

