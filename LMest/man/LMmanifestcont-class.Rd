\name{LMmanifestcont-class}
\alias{LMmanifestcont-class}
\docType{class}


\title{Class \code{'LMmanifestcont'}}

\description{An S3 class object created by \code{\link{lmestCont}} for Latent Markov (LM) model for continuous responses in long format with covariates in the measurement model.}

\value{
	\item{Al}{support points for the latent states}
	\item{Be}{estimate of the vector of regression parameters}
	\item{Si}{estimate of var-cov matrix common to all states}
	\item{piv}{vector of initial probabilities}
	\item{Pi}{transition matrix}
 	\item{lk}{maximum log-likelihood}
 	\item{np}{number of parameters}
 	\item{k}{optimal number of latent states}
 	\item{aic}{value of the Akaike Information Criterion}
 	\item{bic}{value of Bayesian Information Criterion}
 	\item{n}{number of observations in the data}
  \item{TT}{number of time occasions}
  \item{modBasic}{model on the transition probabilities (0 for time-heter., 1 for time-homog., from 2 to (TT-1) partial homog. of that order)}
  \item{lkv}{log-likelihood trace at every step}
 	\item{seAl}{standard errors for the support points Al}
 	\item{seBe}{standard errors regression parameters Be}
 	\item{sepiv}{standard errors for the initial probabilities}
 	\item{sePi}{	
standard errors for the transition probabilities}
\item{seSi}{standard errors for the var-cov matrix}
 	  \item{Lk}{vector containing the values of the log-likelihood of the LM model for each \code{k} (latent states)}
  \item{Np}{vector containing the number of parameters for each \code{k} (latent states)}
   	\item{Bic}{vector containing the values of the BIC for each \code{k}}
  \item{Aic}{vector containing the values of the AIC for each \code{k}}
 	\item{J}{information matrix}
 	\item{sc}{score vector}
  \item{V}{array containing the posterior distribution of the latent states for each units and time occasion}
 	\item{Ul}{matrix containing the predicted sequence of latent states by the local decoding method}
 	\item{Pmarg}{matrix containing the marginal distribution of the latent states} 
	\item{call}{command used to call the function}
	\item{data}{data frame given in input}
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni
}


\seealso{\code{\link{lmestCont}}}


