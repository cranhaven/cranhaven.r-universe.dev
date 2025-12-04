\name{Survival (CJS)}
\alias{survCJS}
\alias{survCJSaj}
\alias{BsurvCJS}
\title{
Survival from recapture data with Cormack-Jolly-Seber (CJS) model
}
\description{
Calculation of apparent survival (accounting for recapture probability) from mark-recapture data, with time-dependent phi or p, possibly with covariates. Function \code{survCHSaj} allows for different survival parameters for juveniles and adults; juveniles are assumed to become adults after the first interval. \code{BsurvCJS} is a Bayesian version.
}
\usage{
survCJS(DH, model=list(phi~1, p~1), data=NULL, freq=1, group, interval=1,
    ci = 0.95, link=c("logit", "probit"), ...)

survCJSaj(DHj, DHa=NULL, model=list(phiJ~1, phiA~1, p~1), data=NULL,
    freqj=1, freqa=1, ci = 0.95, link=c("logit", "probit"), ...)

BsurvCJS(DH, model=list(phi~1, p~1), data = NULL, freq=1, priors=NULL,
    chains=3, draws=1e4, burnin=1000, thin=1, adapt=1000,
    parallel = NULL, seed=NULL, priorOnly=FALSE, ...)
}
\arguments{
  \item{DH}{
a 1/0 matrix with detection histories with a row for each animal captured and a column for each capture occasion.
}
  \item{model}{
a list of formulae symbolically defining a linear predictor for each parameter in terms of covariates.
}
  \item{data}{
a data frame with a row for each survival interval / recapture occasion and columns for each of the covariates used to estimate phi or p.
}
  \item{freq}{
a scalar or a vector of length \code{nrows(DH)} with the frequency of each detection history. Negative values indicate trap losses.
}
  \item{group}{
an optional factor of length \code{nrows(DH)}; if provided, \code{group} can be included in the model definition, see Examples.
}
  \item{interval}{
the time interval between capture occasions; scalar if all intervals are equal or a vector of length \code{ncols(DH) - 1}; the units used must be the same as those for the apparent survival estimate, eg, for annual survival use years.
}
  \item{DHj, DHa}{
detection history matrices for animals marked as juveniles and adults respectively; DHa should be NULL if no animals were marked as adults.
}
  \item{freqj, freqa}{
frequencies of each detection history in DHj and DHa; freqa is ignored if DHa = NULL.
}
  \item{ci}{
the required confidence interval.
}
  \item{link}{
the link function to use, either logit or probit; see \link{Links}.
}
  \item{...}{
other arguments passed to \code{\link{nlm}}.
}
  \item{priors}{
a list with elements for prior mean and variance for coefficients; see Details.
}
  \item{chains}{
the number of Markov chains to run.
}
  \item{draws}{
the minimum number of values to return; the actual number returned may be slightly higher, as it will be a multiple of \code{chains}.
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
  \item{parallel}{
if TRUE or NULL and sufficient cores are available, the MCMC chains are run in parallel; if TRUE and insufficient cores are available, a warning is given.
}
  \item{seed}{
a positive integer, the seed for the random number generators.
}
}
\details{
\code{BsurvCJS} uses a probit link to model apparent survival and detection as a function of covariates; most software uses a logistic (logit) link.
See \link{Links}.
Coefficients on the probit scale are about half the size of the equivalent on the logit scale.

Priors for \code{BsurvCJS} are listed in the \code{priors} argument, which may contain elements:

\code{muPhi} and \code{muP} : the means for apparent survival and detection coefficients respectively. This may be a vector with one value for each coefficient, including the intercept, or a scalar, which will be used for all. The default is 0.

\code{sigmaPhi} and \code{sigmaP} : the variance for apparent survival and detection coefficients respectively. This may be (1) a vector with one value for each coefficient, including the intercept, which represents the variance, assuming independence, or (2) a scalar, which will be used for all. The function does not currently allow a variance-covariance matrix. The default is 1, which is somewhat informative.

When specifying priors, note that numerical covariates are standardized internally before fitting the model. For an intercept-only model, a prior of Normal(0, 1) on the probit scale implies a Uniform(0, 1) or Beta(1, 1) prior on the probability scale.
}

\value{
\code{survCJS} and \code{survCJSaj} return an object of class \code{wiqid}, a list with elements:

  \item{call}{The call used to produce the results}
  \item{beta }{Estimates of the coefficients in the linear predictors for phi and p.}
  \item{beta.vcv }{The variance-covariance matrix for the beta estimates.}
  \item{real }{Back-transformed estimates of phi and p for each interval / occasion. }
  \item{logLik}{a vector with elements for log(likelihood), number of parameters, and effective sample size. If the variance-covariance matrix cannot be calculated, the second element should be \code{NA}.}

There are \code{print}, \code{logLik}, and \code{nobs} methods for class \code{wiqid}.

\code{BsurvCJS} returns an object of class \code{Bwiqid}, a data frame with columns for each p and psi value containing the series of MCMC draws, and attributes for details of the MCMC run.
}
\references{
Lebreton, J-D; K P Burnham; J Clobert; D R Anderson. 1992. Modeling survival and testing biological hypotheses using marked animals: a unified approach with case studies. \emph{Ecological Monographs} 62:67-118.
}
\author{
Mike Meredith
}

\section{Benchmarks}{
Output of \code{survCJS} has been checked against program MARK with the dipper data set: coefficients are not the same as MARK uses models without an intercept, but the real values agree to 3 decimal places.
}

\examples{
data(dippers)

DH <- dippers[1:7]  # Extract the detection histories
survCJS(DH)  # the phi(.) p(.) model
survCJS(DH, phi ~ .time)  # the phi(t) p(.) model
df <- data.frame(flood = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
survCJS(DH, phi ~ flood, data=df)  # the phi(flood) p(.) model
# Including a grouping factor:
survCJS(DH, phi ~ flood*group, data=df, group=dippers$sex)

# With unequal intervals - suppose no data were collected in year 5:
DH1 <- DH[, -5]
survCJS(DH1, phi ~ .time, interval = c(1, 1, 1, 2, 1))

# See also the examples in the dippers help file.
}
