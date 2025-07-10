\name{mcmcplots-package}
\alias{mcmcplots-package}
\alias{mcmcplots}
\docType{package}

\title{
  Plots for MCMC Output
}

\description{
  Viewing diagnostics plots for MCMC output is often difficult when a Bayesian model has a large number of parameters.  Fitting all density or trace plots in a single plotting window is not possible when the number of parameters is large.  One common solution is to create one plot window at a time and prompt the user before creating each plot. However, clicking through plot windows can be tedious and slow.


  This package attempts to address these problems by providing a function (\code{mcmcplot}) that produces common MCMC diagnostic plots in an html file that can be viewed from a web browser.  When viewed in a web browser, hundreds of MCMC plots can be viewed efficiently by scrolling through the output as if it were any typical web page.


  Also, \code{mcmcplot} and other functions in this package -- \code{denplot}, \code{traplot}, \code{caterplot} -- have arguments that facilitate selecting subsets of parameters to plot.  For example, specifying the \code{parms} argument to be "beta" will prompt any of the previous functions to create plots for parameters with names that start with "beta", such as "beta[1]", "beta[2]", and so on. Additionally, specifying the \code{random} option in any of these functions will produce plots for a random subset of parameters in the model.


  This package also contains other plotting functions which can be useful in developing and debugging MCMC software written in \R.  The \code{denoverplot} function creates overlaying density plots of all common parameters from two different MCMC simulations.  This function can be useful when debugging MCMC software.  MCMC software is sometimes written in stages, where the first stage of development involves writing an MCMC sampler in a high-level programming language like \R\ or WinBUGS.  If the program is too slow to be practical for most data sets, then the second stage of development involves rewriting the MCMC sampler in a low-level language like C or Fortran.  If overlaying density plots show slight differences, then the new, low-level code likely has bugs.


  The \code{corplot} function (see also \code{parcorplot}) creates a "heat plot" of a correlation matrix.  This function can be useful in deciding on a blocking structure for an MCMC algorithm, because highly correlated parameters can be sampled in a single block of an MCMC algorithm to improve efficiency.
}

\author{
  S. McKay Curtis with contributions from Ilya Goldin

  Maintainer: S. McKay Curtis <s.mckay.curtis@gmail.com>

  This research was supported in part by Grant R01 AG 029672 from the National Institute on Aging, Paul K. Crane, PI.
}

\references{
  None.
}

\keyword{ package }

\seealso{
  \pkg{coda}
}

\examples{
\dontrun{
## mcmcplots functions work on bugs objects too
library(R2WinBUGS)
example("openbugs", "R2WinBUGS")
## from the help file for openbugs:
schools.sim <- bugs(data, inits, parameters, model.file,
                    n.chains = 3, n.iter = 5000,
                    program = "openbugs", working.directory = NULL)
caterplot(schools.sim, "theta")
traplot(schools.sim, "theta")
denplot(schools.sim, "theta")
mcmcplot(schools.sim)
}

## Create fake MCMC output
nc <- 10; nr <- 1000
pnames <- c(paste("alpha[", 1:5, "]", sep=""), paste("gamma[", 1:5, "]", sep=""))
means <- rpois(10, 20)
fakemcmc <- coda::as.mcmc.list(
    lapply(1:3,
           function(i) coda::mcmc(matrix(rnorm(nc*nr, rep(means,each=nr)),
                                         nrow=nr, dimnames=list(NULL,pnames)))))

## Use mcmcplot to plot
## the fake MCMC output
\dontrun{
mcmcplot(fakemcmc)
mcmcplot(fakemcmc, "gamma")
mcmcplot(fakemcmc, regex="alpha\\\\[[12]")
mcmcplot(fakemcmc, "gamma", "alpha\\\\[[12]")
mcmcplot(fakemcmc, random=2)
mcmcplot(fakemcmc, random=c(2, 3))
}

## Use traplot to create
## trace plots of fake MCMC data
traplot(fakemcmc)
traplot(fakemcmc, "gamma")
traplot(fakemcmc, "gamma", "alpha\\\\[[12]]$") # all gamma and alpha[1] and alpha[2]

## Use denplot to create
## density plots of fake MCMC data
denplot(fakemcmc)
denplot(fakemcmc, "gamma")
denplot(fakemcmc, "gamma", "alpha\\\\[[12]]$") # all gamma and alpha[1] and alpha[2]

## Use caterplot to create
## caterpillar plots of fake MCMC data
par(mfrow=c(2,2))
caterplot(fakemcmc, "alpha", collapse=FALSE)
caterplot(fakemcmc, "gamma", collapse=FALSE)
caterplot(fakemcmc, "alpha", labels.loc="axis", col="blue")
caterplot(fakemcmc, "gamma", labels.loc="above", col="red")

## Use caterplot to create
## caterpillar plots of fake MCMC data
## with density strips
caterplot(fakemcmc, "alpha", collapse=FALSE, denstrip=TRUE)
caterplot(fakemcmc, "gamma", collapse=FALSE, denstrip=TRUE)
caterplot(fakemcmc, "alpha", labels.loc="axis", col="blue", denstrip=TRUE)
caterplot(fakemcmc, "gamma", labels.loc="above", col="red", denstrip=TRUE)

## Overlay caterplots
caterplot(fakemcmc, "alpha", collapse=TRUE)
caterplot(fakemcmc, "gamma", collapse=TRUE, add=TRUE, cat.shift=-0.3)

## Use denoverplot to create overlaying density plots
## of all parameters in fake MCMC data
fakemcmc2 <- coda::as.mcmc.list(
    lapply(1:3,
           function(i) coda::mcmc(matrix(rnorm(nc*nr, rep(means, each=nr)),
                                         nrow=nr, dimnames=list(NULL,pnames)))))
denoverplot(fakemcmc, fakemcmc2)

## Use corplot to create a "heat plot" of a
## correlation matrix of the fake MCMC draws
corplot(cor(as.matrix(fakemcmc)), cex.axis=0.75)  ## not exciting
Rho1 <- outer(1:10, 1:10, function(i, j) 0.5^(abs(i-j)))
Rho2 <- outer(1:5, 1:5, function(i, j) 0.25^(i!=j))
dat1 <- t(apply(matrix(rnorm(10*1000), 1000, 10), 1,
                function(z, Rho1) crossprod(Rho1, z), Rho1))
dat2 <- t(apply(matrix(rnorm(5*1000), 1000, 5), 1,
                function(z, Rho2) crossprod(Rho2,z), Rho2))
colnames(dat1) <- paste("theta[", 1:10, "]", sep="")
colnames(dat2) <- paste("alpha[", 1:5, "]", sep="")
dat <- cbind(dat1, dat2)
parcorplot(dat, "theta", col=gray(31:0/31), cex.axis=0.75)  ## just theta parameters
parcorplot(dat, col=heat.colors(31), cex.axis=0.75)
parcorplot(dat, col=topo.colors(31), cex.axis=0.75)
parcorplot(dat, col=terrain.colors(31), cex.axis=0.75)
parcorplot(dat, col=cm.colors(31), cex.axis=0.75)
}
