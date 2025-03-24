\name{fh_hetop}
\alias{fh_hetop}

\title{Fit Fay-Herriot Heteroskedastic Ordered Probit (FH-HETOP) Model
  using JAGS}

\description{
  Fits the FH-HETOP model described by Lockwood, Castellano and Shear
  (2018) using the \code{\link{jags}} function in R2jags.
}

\usage{
fh_hetop(ngk, fixedcuts, p, m, gridL, gridU, Xm=NULL, Xs=NULL,
seed=12345, modelfileonly = FALSE, modloc=NULL, ...)
}

\arguments{
  \item{ngk}{
    Numeric matrix of dimension \code{G x K} in which column \code{k} of
    row \code{g} indicates the number of units from group \code{g}
    falling into category \code{k}.
  }

  \item{fixedcuts}{
    A vector of length 2 providing the first two cutpoints, to identify
    the location and scale of the group parameters.  Note that this
    suffices for any \code{K >= 3}.
  }

  \item{p}{
    Vector of length 2 giving degrees of freedom for cubic spline basis
    to parameterize Efron priors for group means and group standard
    deviations; see References.
  }

  \item{m}{
    Vector of length 2 giving number of grid points to
    parameterize Efron priors for group means and group standard
    deviations; see References.
  }

  \item{gridL}{
    Vector of length 2 of lower bounds for grids to
    parameterize Efron priors for group means and group standard
    deviations; see References.
  }

  \item{gridU}{
    Vector of length 2 of upper bounds for grids to
    parameterize Efron priors for group means and group standard
    deviations; see References.
  }

  \item{Xm}{
    Optional matrix of covariates for the group means.
  }

  \item{Xs}{
    Optional matrix of covariates for the log group standard deviations.
  }

  \item{seed}{
    Passed to \code{\link{set.seed}}.
  }

  \item{modelfileonly}{
    If TRUE, function returns location of JAGS model file only, without
    running JAGS.  Default is FALSE.
  }

  \item{modloc}{
    Optional character vector of length 1 providing the
    full path to the name of file where the JAGS model code will be written.
    Defaults to NULL, in which case the code will be written to a
    temporary file.
  }
  
  \item{ ... }{
    Additional arguments to \code{\link{jags}}.
  }
}

\details{

  The function is basically a wrapper for \code{\link{jags}}, building
  model code depending on the specification of the Efron priors and any
  covariates for the group means and group standard deviations.  Details
  on the FH-HETOP model are provided by Lockwood, Castellano and Shear
  (2018).

  Covariates to predict the group means and group log standard
  deviations are optional.  However, \code{Xm} and \code{Xs} must both
  be either NULL, or specified; the current version of this function
  cannot use covariates to predict one set of parameters but not use any
  covariates to predict the other set.  While covariates in general must
  be present or absent simultaneously for the two sets of parameters, it
  is not necessary that the same covariates be used to predict the two
  sets of parameters.  All covariates must be centered so that they sum
  to zero across groups.
}

\value{A object of class \code{rjags}, with additional information
  specific to the FH-HETOP model.  The additional information is stored
  as a list called \code{fh_hetop_extras} with the following components:

  \item{Finfo}{A list containing information used to estimate the population
    distribution of the residuals from the FH-HETOP model.  Note that
    the posterior samples of the parameters defining the residual
    distribution can be found in the \code{BUGSoutput} element of the
    returned object.}

  \item{Dinfo}{A list containing information about the data used to the
    fit the model, including the counts, covariates and fixed cutpoints.}

  \item{waicinfo}{A list containing information about the WAIC for the
    estimated model; see help file for \code{\link{waic_hetop}}.
  }

  \item{est_star_samps}{A list with posterior samples of parameters with
    respect to the 'star' scale which defines the location and scale of
    the group means and standard deviations that corresponds to a marginal
    population mean of zero and marginal population standard deviation of
    1.  Additional details in help file for \code{\link{mle_hetop}}
  }

  \item{est_star_mug}{A dataframe containing various estimates of the
  group means on the 'star' scale, including posterior means,
  Constrained Bayes and Triple-Goal estimates.  Additional details in
  help file for \code{\link{triple_goal}}.}

  \item{est_star_sigmag}{A dataframe containing various estimates of the
  group standard deviations on the 'star' scale, including posterior
  means, Constrained Bayes and Triple-Goal estimates.  Additional
  details in help file for \code{\link{triple_goal}}.}
}

\seealso{
  \code{\link{jags}}
}

\references{
  Efron B. (2016).
  \dQuote{Empirical Bayes deconvolution estimates,}
  \emph{Biometrika} 103(1):1--20.
  
  Lockwood J.R., Castellano K.E. and Shear B.R. (2018).
  \dQuote{Flexible Bayesian models for inferences from coarsened,
    group-level achievement data,}
  \emph{Journal of Educational and Behavioral Statistics.} 43(6):663--692.
}

\author{
  J.R. Lockwood \email{jrlockwood@ets.org}
}

\examples{
set.seed(1001)

## define mean-centered covariates
G  <- 12
z1 <- sample(c(0,1), size=G, replace=TRUE)
z2 <- 0.5*z1 + rnorm(G)
Z  <- cbind(z1 - mean(z1), z2 = z2 - mean(z2))

## define true parameters dependent on covariates
beta_m    <- c(0.3,  0.8)
beta_s    <- c(0.1, -0.1)
mug       <- Z[,1]*beta_m[1] + Z[,2]*beta_m[2] + rnorm(G, sd=0.3)
sigmag    <- exp(0.3 + Z[,1]*beta_s[1] + Z[,2]*beta_s[2] + 0.2*rt(G, df=7))
cutpoints <- c(-1.0, 0.0, 1.2)

## generate data
ng   <- rep(200,G)
ngk  <- gendata_hetop(G, K = 4, ng, mug, sigmag, cutpoints)
print(ngk)

## fit FH-HETOP model including covariates
## NOTE: using an extremely small number of iterations for testing,
##       so that convergence is not expected
m <- fh_hetop(ngk, fixedcuts = c(-1.0, 0.0), p = c(10,10),
              m = c(100, 100), gridL = c(-5.0, log(0.10)),
              gridU = c(5.0, log(5.0)), Xm = Z, Xs = Z,
              n.iter = 100, n.burnin = 50)

print(m)
print(names(m$fh_hetop_extras))

s <- m$BUGSoutput$summary
print(data.frame(truth = c(beta_m, beta_s), s[grep("beta", rownames(s)),]))

print(cor(mug,    s[grep("mu",    rownames(s)),"mean"]))
print(cor(sigmag, s[grep("sigma", rownames(s)),"mean"]))

## manual calculation of WAIC (see help file for waic_hetop)
tmp <- waic_hetop(ngk, m$BUGSoutput$sims.matrix)
identical(tmp, m$fh_hetop_extras$waicinfo)
}

\keyword{models}
