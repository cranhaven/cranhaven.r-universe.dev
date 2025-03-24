\name{MargCond}
\alias{MargCond}
\alias{print.MargCond}
\alias{summary.MargCond}
\alias{print.summary.MargCond}
\alias{Ebi}
\alias{Vbi}
\alias{expandR}
\alias{getUi}
\alias{reForm}
\alias{reScale}
\title{
  Function to fit joint marginal-conditional models for longitudinal multivariate data.
}
\description{
  Produces an object of class \code{"MargCond"} which is a marginal-conditional multivariate model.
}
\usage{
MargCond(formula, data, ID, tol = 1e-04, max.iter = 50, 
       corstr = "independence", silent = F)
}
\arguments{
  \item{formula}{
  a two-sided linear formula object similar to those in \code{\link{lmer}}.
}
  \item{data}{
  a data frame in which to interpret the variables occuring in the \code{formula}.
}
  \item{ID}{
  a vector which identifies the clusters. The length of \code{ID} should be the same as the number of observations. Data are assumed to be sorted so that observations on a cluster are contiguous rows for all entities in the formula.
}
  \item{tol}{
  the tolerance used in the fitting algorithm.
}
  \item{max.iter}{
  the maximum number of iterations for the ES algorithm.
}
  \item{corstr}{
  a character string specifying the correlation structure.
  The following are permitted:
     \code{"independence"},
     \code{"fixed"},
     \code{"stat_M_dep"},
     \code{"non_stat_M_dep"},
     \code{"exchangeable"},
     \code{"AR-M"} and
     \code{"unstructured"}
}
  \item{silent}{
  a logical variable controlling whether an indication at each
  iteration is printed.
}
}
\details{
The joint marginal-conditional model 

Care should be taken when specifying the random effects structure (see the singular models section of \url{https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html}). As initial estimates for the expectation-substitution algorithm are taken from the univariate mixed model fits, we recommend that these models be fit separately first and examined to ensure that they are not over parameterized. 
}
\value{
  An object of class \code{"MargCond"} representing the fit.
  
  An object of class \code{"MargCond"} is a list containing the following components:
  
  \item{coefficients}{
  a named vector of coefficients.
}
  \item{sigma}{
  a named vector of outcome error standard deviations.
}
  \item{SE}{
  a vector of coefficient, random effect, and error standard deviations.
}
  \item{residuals}{
  the residuals, that is response minus fitted values.
}
  \item{working.correlation}{
  the working correlation returned by the GEE step at convergence.
}
  \item{rand.eff}{
  the random effect covariance matrix.
}
  \item{outcomes}{
  vector of outcome names
}
  \item{Call}{
  the matched call.
}
  \item{v.cov}{
  the scaled covariance matrix of theta
}
  \item{obs}{
  the total number of observations
}
  \item{groups}{
  the total number of clusters
}
  \item{converge}{
  logical indicator of whether the expectation-substitution algorithm converged (i.e. the difference between each element of theta from the previous iteration is smaller than \code{tol}, and the number of iterations is less than \code{max.iter}).
}
}
\references{
Proudfoot J. A., Faig W., Natarajan L., and Xu R. (2018) A joint marginal-conditional model for multivariate longitudinal data. \emph{Statistics in Medicine.} \url{https://doi.org/10.1002/sim.7552}
}

\seealso{
  \code{\link{gee}}, \code{\link{lmer}}.
}

\examples{
set.seed(2112)
NN = 80
n_times = 1:3

## Simulating some data
simdat <- simDat(n = NN, 
                 fixed_effects = list(c(1, 1, 2), c(1.5, 1, 3)), 
                 rand_effects = list(1, 1),
                 error_var = c(4, 4),
                 error_structure = 'normal',
                 rho = .35,
                 times = n_times,
                 X = cbind(rep(1, NN * length(n_times)), 
                           rnorm(NN * length(n_times), 0, 2), 
                           rbinom(NN * length(n_times), 1, .5)),
                 Z = cbind(rep(1, NN * length(n_times))))
                  
## Adding random missing values
aa <- sample(1:nrow(simdat), 10, replace = TRUE)
bb <- sample(1:7, 10, replace = TRUE)
for (i in 1:length(aa)) {
  simdat[aa[i], bb[i]] <- NA
}

## A fit for this simulated multivariate longitudinal data, 
## including a random intercept and exchangeable correlation 
## structure.
summary(MargCond(c(outcome1, outcome2) ~ X2 + X3 + (1 | ID), 
        data = simdat, ID = simdat$ID, corstr = "exchangeable"))
}

\keyword{models}
\keyword{regression}
\keyword{multivariate}