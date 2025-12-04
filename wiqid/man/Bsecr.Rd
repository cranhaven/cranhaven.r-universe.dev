\name{Bayesian SCR}
\alias{Bsecr0}


\title{
Spatially explicit capture-recapture (secr) density estimation using MCMC
}
\description{
Functions to estimate density from mark-recapture data using MCMC methods and JAGS.
}
\usage{
Bsecr0(capthist, buffer = 100, start = NULL, nAug = NA, maxSig = 2*buffer,
                    chains=3, draws=1e4, burnin=0, thin=1, adapt=1000,
                    priorOnly=FALSE, parallel=NULL, seed=NULL, ...)
}
\arguments{
  \item{capthist}{
a \code{capthist} object as defined in package \code{secr} including capture data and detector (trap) layout }
  \item{buffer}{
scalar mask buffer radius (default 100 m)
}
  \item{start}{
an optional object of class \code{secr}, ie, output from the \code{secr.fit} function in package \code{secr}; objects of other classes are silently ignored.
}
  \item{nAug}{
number of individuals in the augmented population; if NA, a suitable default is chosen based on the object passed to \code{start} or a preliminary run of \code{secr.fit}.
}
  \item{maxSig}{
maximum value for the scale parameter of the detection function: the prior is \emph{Uniform(0, maxSig)}.
}
  \item{chains}{
the number of Markov chains to run.
}
  \item{draws}{
the total number of values to return. The number of values calculated per chain is \code{adapt + burnin + ceiling(draws / chains) * thin}.
}
  \item{burnin}{
the number of values to discard at the beginning of each chain.
}
  \item{thin}{
the thinning rate. If set to n > 1, n values are calculated for each value returned.
}
  \item{adapt}{
the number of iterations to run in the JAGS adaptive phase.
}
  \item{priorOnly}{
if TRUE, the function produces random draws from the appropriate \emph{prior} distributions, with a warning.
}
  \item{seed}{
set a seed for the random number generators.
}
  \item{parallel}{
if TRUE or NULL and sufficient cores are available, the MCMC chains are run in parallel; if TRUE and insufficient cores are available, a warning is given.
}
  \item{...}{
other arguments to pass to the function.
}
}
\details{
\code{Bsecr0} implements an intercept-only model (D ~ 1, g0 ~ 1, sigma ~ 1).

}
\value{
Returns an object of class \code{Bwiqid}, data frame with one column for each parameter, ie. D, lam0 and sigma.

There are print, plot, and window methods for \code{Bwiqid}.
}

\references{
Borchers & Efford (2008) Spatially explicit maximum likelihood methods for capture-recapture studies \emph{Biometrics} 64, 377-385

Royle & Dorazio (2008) \emph{Hierarchical Modeling and Inference in Ecology}. Academic Press
}

\author{
Mike Meredith
}
\seealso{
The function \code{secr.fit} in package \code{secr}.
}
\examples{
\donttest{
if(requireNamespace("secr") && requireNamespace("rjags")) {
  # The stoats data set in 'secr'
  data(stoatDNA, package="secr")
  # This takes ca 10 mins on a multicore machine:
  Bout <- Bsecr0(stoatCH, buffer=1000, chains=2) # 2 chains for testing
  summary(Bout)
  plot(Bout)
  # look at diagnostic plots to see if D is constrained by nAug:
  diagPlot(Bout)      # Upper values of D doesn't look constrained.
  plotACs(Bout, 1:20) # Plot the ACs for captured animals
}
}
}
