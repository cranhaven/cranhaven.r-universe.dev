\name{est_mc_basic}

\alias{est_mc_basic}

\title{Estimate basic Markov chain (MC) model}

\description{Main function for estimating the basic MC model. \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{lmestMc}} \bold{function}.}

\usage{est_mc_basic(S, yv, mod = 0, tol = 10^-8, maxit = 1000, out_se = FALSE)}

\arguments{
  \item{S}{matrix  (n x TT) of available configurations of the response variable with categories starting from 0}
  \item{yv}{vector of frequencies of the available configurations}
  \item{mod}{model on the transition probabilities (0 for time-heter., 1 for time-homog., from 2 to (TT-1) partial homog. of that order)}
  \item{tol}{tolerance level for convergence}
  \item{maxit}{maximum number of iterations of the algorithm}
  \item{out_se}{to compute the information matrix and standard errors}
}

\value{
  \item{lk }{maximum log-likelihood}
  \item{piv }{estimate of initial probability vector}
  \item{Pi }{estimate of transition probability matrices}
  \item{np }{number of free parameters}
  \item{aic }{value of AIC for model selection}
  \item{bic }{value of BIC for model selection}
  \item{Fy }{estimated marginal distribution of the response variable for each time occasion}
  \item{sepiv}{standard errors for the initial probabilities}
  \item{sePi}{standard errors for the transition probabilities}
  \item{call}{command used to call the function}
}

\references{Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data},
	Chapman and Hall/CRC press.}
\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia (IT), http://www.stat.unipg.it/bartolucci}

\examples{
# Example of drug consumption data

# load data
data(data_drug)
data_drug <- as.matrix(data_drug)
S <- data_drug[,1:5]-1
yv <- data_drug[,6]

# fit of the Basic MC model
out <- est_mc_basic(S, yv, mod = 1, out_se = TRUE)
summary(out)
}
