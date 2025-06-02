\name{LMmixed-class}
\alias{LMmixed-class}
\docType{class}

\title{Class \code{'LMmixed'}}

\description{An S3 class object created by \code{\link{lmestMixed}} for the mixed latent Markov (LM) models for categorical data in long format.}

\value{
   \item{la}{estimate of the mass probability vector (distribution of the random effects)}
  \item{Piv}{estimate of initial probabilities}
  \item{Pi}{estimate of transition probability matrices}
  \item{Psi}{estimate of conditional response probabilities}
  \item{lk}{maximum log-likelihood}
  \item{W}{posterior probabilities of the random effect}
  \item{np}{number of free parameters}
  \item{k1}{number of support points (latent classes) of the latent variable defining the unobserved clusters}
  \item{k2}{number of support points (latent states) of the latent variable defining the first-order Markov process}
  \item{bic}{value of the Akaike Information Criterion for model selection}
  \item{aic}{value of the Akaike Information Criterion for model selection}
  \item{n}{number of observations in the data}
  \item{TT}{number of time occasions}
  \item{sela}{standard errors for \code{la}}
  \item{sePiv}{estimate of initial probability matrix}
  \item{sePi}{standard errors for the transition probabilities}
  \item{sePsi}{standard errors for the conditional response matrix}
  \item{call}{command used to call the function}
  \item{data}{the input data}
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}


\seealso{\code{\link{lmestMixed}}}


