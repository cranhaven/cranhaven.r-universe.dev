\name{LMbasiccont-class}
\alias{LMbasiccont-class}
\docType{class}


\title{Class \code{'LMbasiccont'}}

\description{An S3 class object created by \code{\link{lmestCont}} function for the latent Markov (LM) model for continuous responses in long format.}

\value{
  \item{lk}{maximum log-likelihood}
   \item{piv}{estimate of initial probability vector}
  \item{Pi}{estimate of transition probability matrices (k x k x TT) }
  \item{Mu}{estimate of conditional means of the response variables (r x k)}
  \item{Si}{estimate of var-cov matrix common to all states (r x r)}
  \item{np}{number of free parameters}
    \item{k}{optimal number of latent states}
  \item{aic}{value of the Akaike Information Criterion for model selection}
  \item{bic}{value of the Bayesian Information Criterion for model selection}
 \item{lkv}{log-likelihood trace at every step}
  	\item{n}{number of observations in the data}
  \item{TT}{number of time occasions}
   \item{modBasic}{model on the transition probabilities: default 0 for time-heterogeneous transition matrices, 1 for time-homogeneous transition matrices, 2 for partial time homogeneity based on two transition matrices one from 2 to (TT-1) and the other for TT}
   \item{sepiv}{standard errors for the initial probabilities}
   \item{sePi}{standard errors for the transition probabilities}
   \item{seMu}{standard errors for the conditional means}
   \item{seSi}{standard errors for the var-cov matrix}
   \item{sc}{score vector}
   \item{J}{information matrix}
   \item{Lk}{vector containing the values of the log-likelihood of the LM model with each \code{k} (latent states)}
 	\item{Bic}{vector containing the values of the BIC of the LM model with each \code{k} (latent states)}
  \item{Aic}{vector containing the values of the AIC of the LM model with each \code{k} (latent states)}
 \item{V}{array containing the posterior distribution of the latent states for each units and time occasion}
 	\item{Ul}{matrix containing the predicted sequence of latent states by the local decoding method}
 	\item{Pmarg}{matrix containing the marginal distribution of the latent states} 
  \item{call}{command used to call the function}
  \item{data}{data frame given in input}
}


\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini }


\seealso{\code{\link{lmestCont}}}
