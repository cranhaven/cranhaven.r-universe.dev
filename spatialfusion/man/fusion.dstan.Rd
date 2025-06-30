\name{fusion.dstan}
\alias{fusion.dstan}
\title{Fit a spatial fusion model using Stan}
\usage{
\method{fusion}{dstan}(data, n.latent = 1, bans = 0, pp.offset,
    verbose = FALSE, prior.pointbeta, prior.areabeta,
    prior.tausq, prior.phi, prior.z,
    nsamples = 2000, nburnin = 1000, thinning = 1,
    nchain = 2, ncore = 2, adapt.delta = 0.95, ...)
}
\arguments{
\item{data}{an object of class \code{dstan}. Output of \code{\link{fusionData}()}.}

\item{n.latent}{integer. Number of latent processes to be modeled.}

\item{bans}{either 0 or a matrix of 0s and 1s with dimension J times n.latent, where J is the total number of response variables. If \code{matrix}, 1 indicates banning an association between the latent process and response variable.If 0, no association is banned.}

\item{pp.offset}{numeric, vector of numeric or matrix of numeric. Offset term for point pattern data.}

\item{verbose}{logical. If TRUE, prints progress and debugging information.}

\item{prior.pointbeta}{a list with prior information for the coeffients of geostatistical model component. The default prior is \code{list(distr = "normal", pars = c(0, 10))}, i.e. a normal distribution with mean 0 and standard devation 10.}

\item{prior.areabeta}{a list with prior information for the coeffients of lattice model component. The default prior is \code{list(distr = "normal", pars = c(0, 10))}, i.e. a normal distribution with mean 0 and standard devation 10.}

\item{prior.tausq}{a list with prior information for the coeffients of geostatistical model component. The default prior is \code{list(distr = "inv_gamma", pars = c(2, 1))}, i.e. a inverse gamma distribution with shape 2 and rate 1.}

\item{prior.phi}{a list with prior information for the spatial range parameter. NO default prior is available. We recomend using a moderately informative normal prior.}

\item{prior.z}{a list with prior information for the design matrix, which also controls the partial sill. The default prior is \code{list(distr = "normal", pars = c(1, 1))}, i.e. a normal distribution with mean 1 and standard devation 1.}

\item{nsamples}{a positive integer specifying the number of samples for each chain (including burn-in samples). Default 2000.}

\item{nburnin}{a positive integer specifying the number of burn-in samples. Default 1000.}

\item{thinning}{a positive integer specifying the thinning parameter. Default 1.}

\item{nchain}{a positive integer specifying the number of chains. Default 2.}

\item{ncore}{a positive integer specifying the number of cores to use when executing the chains in parallel. Default 2.}

\item{adapt.delta}{a numeric value between 0 and 1 specifying the target acceptance rate. Default 0.95.}

\item{...}{additional arguments passed to \code{sampling} function in \pkg{rstan}}
}

\value{
The returned value is a list consists of

\item{model}{an object of S4 class \code{stanfit} representing the fitted Stan model}
\item{data}{the data structure used to fit the model}
}

\description{
Fit a spatial fusion model using Stan based on the unifying framework proposed by Wang and Furrer (2021). One or more latent Gaussian process(es) is assumed to be associated with the spatial response variables.
}

\details{
In the model parameterization, \code{beta} are fixed-effect coefficients, \code{phi} is the range parameter, \code{Z_ij} is the ith row and j column of the design matrix for latent processes and \code{tau_sq} is the variance parameter of a normal distribution.}

\note{Only exponential covariance model for the latent processes is implemented. However, it can be easily extended by modifying the model code from the output.}


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
}
\keyword{models}

