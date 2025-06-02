\name{MCcov-class}
\alias{MCcov-class}
\docType{class}


\title{Class \code{'MCcov'}}

\description{An S3 class object created by \code{\link{lmestMc}} function for Markov chain (MC) model for categorical responses in long format with covariates.}

\value{
  \item{lk}{maximum log-likelihood}
    \item{Be}{estimated array of the parameters affecting the logit for the initial probabilities}
  \item{Ga}{estimated array of the parameters affecting the logit for the transition probabilities}
 \item{np}{number of free parameters}
 \item{aic}{value of the Akaike Information Criterion (AIC) for model selection}
 \item{bic}{value of the Bayesian Information Criterion (BIC) for model selection}
   \item{n}{number of observations in the data}
  \item{TT}{number of time occasions}
 \item{seBe}{standard errors for \code{Be}}
 \item{seGa}{standard errors for \code{Ga}}
  \item{Piv}{estimate of initial probability matrix}
  \item{PI}{estimate of transition probability matrices}
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


