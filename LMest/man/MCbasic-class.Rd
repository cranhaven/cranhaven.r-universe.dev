\name{MCbasic-class}
\alias{MCbasic-class}
\docType{class}


\title{Class \code{'MCbasic'}}

\description{An S3 class object created by \code{\link{lmestMc}} function for the Markov chain (MC) model without covariates.}

\value{
  \item{lk}{maximum log-likelihood}
  \item{piv}{estimate of initial probability vector}
  \item{Pi}{estimate of transition probability matrices}
  \item{np}{number of free parameters}
   \item{aic}{value of the Akaike Information Criterion for model selection}
  \item{bic}{value of the Bayesian Information Criterion for model selection}
  \item{Fy}{estimated marginal distribution of the response variable ats each time occasion}
    \item{n}{number of observations in the data}
  \item{TT}{number of time occasions}
  \item{modBasic}{model on the transition probabilities: default 0 for time-heterogeneous transition matrices, 1 for time-homogeneous transition matrices, 2 for partial time homogeneity based on two transition matrices one from 2 to (TT-1) and the other for TT}
  \item{sepiv}{standard errors for the initial probabilities}
  \item{sePi}{standard errors for the transition probabilities}
  \item{call}{command used to call the function}
    \item{data}{data frame given in input}
}


\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}


\seealso{\code{\link{lmestMc}}}


