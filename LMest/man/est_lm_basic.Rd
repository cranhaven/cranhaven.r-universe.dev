\name{est_lm_basic}

\alias{est_lm_basic}

\title{Estimate basic LM model}

\description{Main function for estimating the basic LM model.  \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{lmest}} \bold{function}.}

\usage{est_lm_basic(S, yv, k, start = 0, mod = 0, tol = 10^-8, maxit = 1000,
	                  out_se = FALSE, piv = NULL, Pi = NULL, Psi = NULL)}

\arguments{
  \item{S}{array of available configurations (n x TT x r) with categories starting from 0 (use NA for missing responses)}
  \item{yv}{vector of frequencies of the available configurations}
  \item{k}{number of latent states}
  \item{start}{type of starting values (0 = deterministic, 1 = random, 2 = initial values in input)}
  \item{mod}{model on the transition probabilities (0 for time-heter., 1 for time-homog., from 2 to (TT-1) partial homog. of that order)}
  \item{tol}{tolerance level for convergence}
  \item{maxit}{maximum number of iterations of the algorithm}
  \item{out_se}{to compute the information matrix and standard errors}
  \item{piv }{initial value of the initial probability vector (if start=2)}
  \item{Pi }{initial value of the transition probability matrices (k x k x TT) (if start=2) }
  \item{Psi }{initial value of the conditional response probabilities (mb x k x r) (if start=2)}
}

\value{
  \item{lk }{maximum log-likelihood}
  \item{piv }{estimate of initial probability vector}
  \item{Pi }{estimate of transition probability matrices}
  \item{Psi }{estimate of conditional response probabilities}
  \item{np }{number of free parameters}
  \item{aic }{value of AIC for model selection}
  \item{bic }{value of BIC for model selection}
  \item{lkv }{log-likelihood trace at every step}
  \item{V }{array containing the posterior distribution of the latent states for each response configuration and time occasion}
  \item{sepiv}{standard errors for the initial probabilities}
  \item{sePi}{standard errors for the transition probabilities}
  \item{sePsi}{standard errors for the conditional response probabilities}
  \item{call}{command used to call the function}
}

\references{Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data},
	Chapman and Hall/CRC press.}
\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia (IT), http://www.stat.unipg.it/bartolucci}

\examples{
\dontrun{
# Example of drug consumption data

# load data
data(data_drug)
data_drug <- as.matrix(data_drug)
S <- data_drug[,1:5]-1
yv <- data_drug[,6]

# fit of the Basic LM model
k <- 3
out <- est_lm_basic(S, yv, k, mod = 1)
summary(out)

# Example based on criminal data

# load criminal data
data(data_criminal_sim)
out <- long2wide(data_criminal_sim, "id" , "time" , "sex",
c("y1","y2","y3","y4","y5","y6","y7","y8","y9","y10"),aggr = T, full = 999)
XX <- out$XX
YY <- out$YY
freq <- out$freq

# fit basic LM model with increasing number of states to select the most suitable
Res0 <- vector("list", 7)
for(k in 1:7){
    Res0[[k]] <- est_lm_basic(YY, freq, k, mod = 1, tol = 10^-4)
    save(list <- ls(), file = "example_criminal_temp.RData")
}
out1 <- Res0[[6]]
}
}
