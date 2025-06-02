\name{LMmanifest-class}
\alias{LMmanifest-class}
\docType{class}


\title{Class \code{'LMmanifest'}}

\description{An S3 class object created by \code{\link{lmest}} for Latent Markov (LM) model with covariates in the measurement model.}

\value{
\item{mu}{vector of cut-points}
	\item{al}{support points for the latent states}
	\item{be}{estimate of the vector of regression parameters}
	\item{si}{sigma of the AR(1) process (mod = "FM")}
	\item{rho}{parameter vector for AR(1) process (mod = "FM")}
	\item{la}{vector of initial probabilities}
	\item{PI }{transition matrix}
 	\item{lk}{maximum log-likelihood}
 	\item{np}{number of parameters}
 	\item{k}{optimal number of latent states}
 	\item{aic}{value of the Akaike Information Criterion}
 	\item{bic}{value of Bayesian Information Criterion}
 	\item{n}{number of observations in the data}
  \item{TT}{number of time occasions}
  \item{modManifest}{for LM model with covariates on the manifest model: "LM" = Latent Markov with stationary transition, "FM" = finite mixture model where a mixture of AR(1) processes is estimated with common variance and specific correlation coefficients}
 	\item{sebe}{standard errors for the regression parameters be}
 	\item{selrho}{standard errors for logit type transformation of rho}
 	\item{J1}{information matrix}
 	\item{V}{array containing the posterior distribution of the latent states for each units and time occasion}
 	\item{PRED1}{prediction of the overall latent effect}
 	   	\item{S}{array containing the available response configurations}
 	  \item{yv}{vector of frequencies of the available configurations}
 	  \item{Pmarg}{matrix containing the marginal distribution of the latent states}
 	   \item{Lk}{vector containing the values of the log-likelihood of the LM model with each \code{k} (latent states)}
 	\item{Bic}{vector containing the values of the BIC for each \code{k}}
  \item{Aic}{vector containing the values of the AIC for each \code{k}}
	\item{call}{command used to call the function}
	\item{data}{data frame given in input}
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}


\seealso{\code{\link{lmest}}}


