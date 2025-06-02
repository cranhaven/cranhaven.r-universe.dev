\name{LMbasic-class}
\alias{LMbasic-class}
\docType{class}

\title{Class \code{'LMbasic'}}

\description{An S3 class object created by \code{\link{lmest}} function for basic Latent Markov (LM) model.}

\value{
\item{lk}{maximum log-likelihood at convergence of the EM algorithm}
\item{piv}{estimate of initial probability vector}
  \item{Pi}{estimate of transition probability matrices (k x k x TT)}
  \item{Psi}{estimate of conditional response probabilities (mb x k x r)}
  \item{np}{number of free parameters}
  \item{k}{optimal number of latent states}
  \item{aic}{value of the Akaike Information Criterion for model selection}
  \item{bic}{value of the Bayesian Information Criterion for model selection}
  \item{lkv}{log-likelihood trace at every step}
  \item{n}{sample size (sum of the weights when weights are provided)}
  \item{TT}{number of time occasions}
  \item{modBasic}{model on the transition probabilities: default 0 for time-heterogeneous transition matrices, 1 for time-homogeneous transition matrices, 2 for partial time homogeneity based on two transition matrices one from 2 to (TT-1) and the other for TT.}
  \item{sepiv}{standard errors for the initial probabilities}
  \item{sePi}{standard errors for the transition probabilities}
  \item{sePsi}{standard errors for the conditional response probabilities}
     \item{Lk}{vector containing the values of the log-likelihood of the LM model with each \code{k} (latent states)}
 	\item{Bic}{vector containing the values of the BIC for each \code{k}}
  \item{Aic}{vector containing the values of the AIC for each \code{k}}
    \item{V}{array containing the estimated posterior probabilities of the latent states for each response configuration and time occasion}
 	\item{Ul}{matrix containing the predicted sequence of latent states by the local decoding method}
 	\item{S}{array containing the available response configurations}
 	\item{yv}{vector of frequencies of the available configurations}
 	\item{Pmarg}{matrix containing the marginal distribution of the latent states}
 	\item{ns}{number of distinct response configurations}
  \item{call}{command used to call the function}
  \item{data}{data.frame given in input}
  
}


\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}

\seealso{\code{\link{lmest}}}


