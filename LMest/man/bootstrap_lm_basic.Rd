\name{bootstrap_lm_basic}

\alias{bootstrap_lm_basic}

\title{Parametric bootstrap for the basic LM model}

\description{Function that performs bootstrap parametric resampling to compute standard errors for the parameter estimates. \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{bootstrap}} \bold{function}.}

\usage{bootstrap_lm_basic(piv, Pi, Psi, n, B = 100, start = 0, mod = 0, tol = 10^-6)}

\arguments{
  \item{piv }{initial probability vector}
  \item{Pi }{probability transition matrices (k x k x TT)}
  \item{Psi }{matrix of conditional response probabilities (mb x k x r)}
  \item{n }{sample size}
  \item{B }{number of bootstrap samples}
  \item{start}{type of starting values (0 = deterministic, 1 = random)}
  \item{mod}{model on the transition probabilities (0 for time-heter., 1 for time-homog., from 2 to (TT-1) partial homog. of that order)}
  \item{tol}{tolerance level for convergence}
}

\value{
  \item{mPsi }{average of bootstrap estimates of the conditional response probabilities}
  \item{mpiv }{average of bootstrap estimates of the initial probability vector}
  \item{mPi }{average of bootstrap estimates of the transition probability matrices}
  \item{sePsi }{standard errors for the conditional response probabilities}
  \item{sepiv }{standard errors for the initial probability vector}
  \item{sePi }{standard errors for the transition probability matrices}
}

\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia (IT), http://www.stat.unipg.it/bartolucci}

\examples{
\dontrun{
# Example of drug consumption data
# load data
data(data_drug)
data_drug <- as.matrix(data_drug)
S <- data_drug[,1:5]-1
yv <- data_drug[,6]
n <- sum(yv)
# fit of the Basic LM model
k <- 3
out1 <- est_lm_basic(S, yv, k, mod = 1, out_se = TRUE)
out2 <- bootstrap_lm_basic(out1$piv, out1$Pi, out1$Psi, n, mod = 1, B = 1000)
}
}
