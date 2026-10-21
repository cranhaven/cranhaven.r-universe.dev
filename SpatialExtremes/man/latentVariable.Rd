\name{latent}
\alias{latent}
\title{
  Bayesian hierarchical models for spatial extremes
}
\description{
  This function generates a Markov chain from a Bayesian hierarchical
  model for block maxima assuming conditional independence.
}
\usage{
latent(data, coord, cov.mod = "powexp", loc.form, scale.form,
shape.form, marg.cov = NULL, hyper, prop, start, n = 5000, thin = 1,
burn.in = 0, use.log.link = FALSE)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{cov.mod}{A character string corresponding to the covariance
    model for the Gaussian latent processes. Must be one of "gauss" for
    the Smith's model; "whitmat", "cauchy", "powexp" or "bessel" or
    for the Whittle-Matern, the Cauchy, the Powered Exponential and the
    Bessel correlation families.} 
  \item{loc.form, scale.form, shape.form}{R formulas defining the
    spatial linear model for the mean of the latent processes.}
  \item{marg.cov}{Matrix with named columns giving additional covariates
    for the latent processes means. If \code{NULL}, no extra covariates are
    used.}
  \item{hyper}{A named list specifying the hyper-parameters --- see Details.}
  \item{prop}{A named list specifying the jump sizes when a
    Metropolis--Hastings move is needed --- see Details.}
  \item{start}{A named list specifying the starting values --- see Details.}
  \item{n}{The effective length of the simulated Markov chain i.e., once
    the burnin period has been discarded and after thinning.}
  \item{thin}{An integer specifying the thinning length. The default is
    1, i.e., no thinning.}
  \item{burn.in}{An integer specifying the burnin period. The default is
    0, i.e., no burnin.}
  \item{use.log.link}{An integer. Should a log-link function should be
    use for the GEV scale parameters, i.e., assuming that the GEV scale
    parameters a drawn from a log-normal process rather than a gaussian
    process.}
}
\details{
  This function generates a Markov chain from the following model. For
  each \eqn{x \in R^d}{x in R^d}, suppose that
  \eqn{Y(x)}{Y(x)} is GEV distributed whose parameters \eqn{\{\mu(x),
    \sigma(x), \xi(x)\}}{{\mu(x), \sigma(x), \xi(x)}} vary smoothly for
  \eqn{x \in R^d}{x in R^d} according to a stochastic process
  \eqn{S(x)}{S(x)}. We assume that the processes for each GEV
  parameters are mutually independent Gaussian processes. For instance,
  we take for the location parameter \eqn{\mu(x)}{\mu(x)}
  \deqn{\mu(x) = f_{\mu(x)}(x;\beta_\mu) + S_\mu(x;\alpha_{\mu},
    \lambda_\mu, \kappa_\mu)}{\mu(x) = f_{\mu(x)}(x;\beta_\mu) +
    S_\mu(x; \alpha_\mu, \lambda_\mu, \kappa_\mu)}
  where \eqn{f_\mu}{f_\mu} is a deterministic function depending on
  regression parameters \eqn{\beta_\mu}{\beta_\mu}, and
  \eqn{S_\mu}{S_\mu} is a zero mean, stationary Gaussian process with a
  prescribed covariance function with sill \eqn{\alpha_\mu}{\alpha_\mu},
  range \eqn{\lambda_\mu}{\lambda_\mu} and shape parameters
  \eqn{\kappa_\mu}{\kappa_\mu}. Similar formulations for the scale
  \eqn{\sigma(x)}{\sigma(x)} and the shape \eqn{\xi(x)}{\xi(x)} parameters
  are used. Then conditional on the values of the three Gaussian
  processes at the sites \eqn{(x_1, \ldots, x_K)}{(x_1, \dots, x_K)},
  the maxima are assumed to follow GEV distributions
  \deqn{Y_i(x_j) \mid \{\mu(x_j), \sigma(x_j), \xi(x_j)\} \sim
    \mbox{GEV}\{\mu(x_j), \sigma(x_j), \xi(x_j)\},}{Y_i(x_j) | {\mu(x_j),
      \sigma(x_j), \xi(x_j)} ~ GEV{\mu(x_j), \sigma(x_j), \xi(s_j)},}
  independently for each location \eqn{(x_1, \ldots, x_K)}{(x_1, \ldots,
    x_K)}.

  A joint prior density must be defined for the sills, ranges, shapes
  parameters of the covariance functions as well as for the regression
  parameters \eqn{\beta_\mu}{\beta_\mu},\eqn{\beta_\sigma}{\beta_\sigma}
  and \eqn{\beta_\xi}{\beta_\xi}. Conjugate priors are used whenever
  possible, taking independent inverse Gamma and multivariate normal
  distributions for the sills and the regression parameters. No
  conjugate prior exist for \eqn{\lambda}{\lambda} and
  \eqn{\kappa}{\kappa}, for wich a Gamma distribution is assumed.

  Consequently \code{hyper} is a named list with named components
  \describe{
    \item{sills}{A list with three components named 'loc', 'scale'
      and 'shape' each of these is a 2-length vector specifying the shape
      and the scale of the inverse Gamma prior distribution for the sill
      parameter of the covariance functions;}
    \item{ranges}{A list with three components named 'loc', 'scale' and
      'shape' each of these is a 2-length vector specifying the shape and
      the scale of the Gamma prior distribution for the range parameter of
      the covariance functions.}
    \item{smooths}{A list with three components named 'loc', 'scale' and
      'shape' each of these is a 2-length vector specifying the shape and
      the scale of the Gamma prior distribution for the shape parameter of
      the covariance functions;}
    \item{betaMean}{A list with three components named 'loc', 'scale'
      and 'shape' each of these is a vector specifying the mean vector of
      the multivariate normal prior distribution for the regression
      parameters;}
    \item{betaIcov}{A list with three components named 'loc', 'scale'
      and 'shape' each of these is a matrix specifying the inverse of
      the covariance matrix of the multivariate normal prior distribution
      for the regression parameters.}
  }

  As no conjugate prior exists for the GEV parameters and the range and
  shape parameters of the covariance functions, Metropolis--Hastings
  steps are needed. The proposals \eqn{\theta_{prop}}{\theta_[prop]} are
  drawn from a proposal density \eqn{q(\cdot \mid \theta_{cur},
    s)}{q(. | \theta_[cur], s)} where \eqn{\theta_{cur}}{\theta_[cur]} is
  the current state of the parameter and \eqn{s}{s} is a parameter of
  the proposal density to be defined. These proposals are driven by
  \code{prop} which is a list with three named components 
  \describe{
    \item{gev}{A vector of length 3 specifying the standard deviations
      of the proposal distributions. These are taken to be normal
      distribution for the location and shape GEV parameters and a
      log-normal distribution for the scale GEV parameters;}
    \item{ranges}{A vector of length 3 specifying the jump sizes for the
      range parameters of the covariance functions --- \eqn{q(\cdot |
	\theta_{cur}, s)}{q(. | \theta_[cur], s)} is the log-normal density
      with mean \eqn{\theta_{cur}}{\theta_[cur]} and standard deviation
      \eqn{s}{s} both on the log-scale;}
    \item{smooths}{A vector of length 3 specifying the jump sizes for
      the shape parameters of the covariance functions --- \eqn{q(\cdot |
	\theta_{cur}, s)}{q(. | \theta_[cur], s)} is the log-normal density
      with mean \eqn{\theta_{cur}}{\theta_[cur]} and standard deviation
      \eqn{s}{s} both on the log-scale.}
  }

  If one want to held fixed a parameter this can be done by setting a null
  jump size then the parameter will be held fixed to its starting value.

  Finally \code{start} must be a named list with 4 named components
  \describe{
    \item{sills}{A vector of length 3 specifying the starting values for
      the sill of the covariance functions;}
    \item{ranges}{A vector of length 3 specifying the starting values
      for the range of the covariance functions;}
    \item{smooths}{A vector of length 3 specifying the starting values
      for the shape of the covariance functions;}
    \item{beta}{A named list with 3 components 'loc', 'scale' and
      'shape' each of these is a numeric vector specifying the starting
      values for the regression coefficients.}
  }
}
\section{Warning}{
  This function can be time consuming and makes an intensive use of BLAS
  routines so it is (much!) faster if you have an optimized BLAS.

  The starting values will never be stored in the generated Markov chain
  even when \code{burn.in=0}.
}
\note{
  If you want to analyze the convergence ans mixing properties of the
  Markov chain, it is recommended to use the library \pkg{coda}.
}
\examples{
\dontrun{
## Generate realizations from the model
n.site <- 30
n.obs <- 50
coord <- cbind(lon = runif(n.site, -10, 10), lat = runif(n.site, -10 , 10))

gp.loc <- rgp(1, coord, "powexp", sill = 4, range = 20, smooth = 1)
gp.scale <- rgp(1, coord, "powexp", sill = 0.4, range = 5, smooth = 1)
gp.shape <- rgp(1, coord, "powexp", sill = 0.01, range = 10, smooth = 1)

locs <- 26 + 0.5 * coord[,"lon"] + gp.loc
scales <- 10 + 0.2 * coord[,"lat"] + gp.scale
shapes <- 0.15 + gp.shape

data <- matrix(NA, n.obs, n.site)
for (i in 1:n.site)
  data[,i] <- rgev(n.obs, locs[i], scales[i], shapes[i])

loc.form <- y ~ lon
scale.form <- y ~ lat
shape.form <- y ~ 1

hyper <- list()
hyper$sills <- list(loc = c(1,8), scale = c(1,1), shape = c(1,0.02))
hyper$ranges <- list(loc = c(2,20), scale = c(1,5), shape = c(1, 10))
hyper$smooths <- list(loc = c(1,1/3), scale = c(1,1/3), shape = c(1, 1/3))
hyper$betaMeans <- list(loc = rep(0, 2), scale = c(9, 0), shape = 0)
hyper$betaIcov <- list(loc = solve(diag(c(400, 100))),
                       scale = solve(diag(c(400, 100))),
                       shape = solve(diag(c(10), 1, 1)))

## We will use an exponential covariance function so the jump sizes for
## the shape parameter of the covariance function are null.
prop <- list(gev = c(1.2, 0.08, 0.08), ranges = c(0.7, 0.8, 0.7), smooths = c(0,0,0))
start <- list(sills = c(4, .36, 0.009), ranges = c(24, 17, 16), smooths
              = c(1, 1, 1),  beta = list(loc = c(26, 0.5), scale = c(10, 0.2),
                               shape = c(0.15)))

mc <- latent(data, coord, loc.form = loc.form, scale.form = scale.form,
             shape.form = shape.form, hyper = hyper, prop = prop, start = start,
             n = 10000, burn.in = 5000, thin = 15)
mc
}
}

\value{A list}
\references{
  Banerjee, S., Carlin, B. P., and Gelfand, A. E. (2004). Hierarchical
  Modeling and Analysis for Spatial Data. Chapman & Hall/CRC, New
  York.
  
  Casson, E. and Coles, S. (1999) Spatial regression models for
  extremes. \emph{Extremes} \bold{1},449--468.
  
  Cooley, D., Nychka, D. and Naveau, P. (2007) Bayesian spatial
  modelling of extreme precipitation return levels \emph{Journal of the
    American Statistical Association} \bold{102}:479, 824--840.

  Davison, A.C., Padoan, S.A. and Ribatet, M. Statistical Modelling of
  Spatial Extremes. Submitted.
}
\author{Mathieu Ribatet}
\keyword{htest}

