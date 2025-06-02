\name{est_mc_cov}

\alias{est_mc_cov}

\title{Estimate Markov chain (MC) model with covariates}

\description{Main function for estimating the MC model with covariates. \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{lmestMc}} \bold{function}.}

\usage{est_mc_cov(S, X1 = NULL, X2 = NULL, yv = rep(1,nrow(S)), start = 0, tol = 10^-8,
	   maxit = 1000, out_se = FALSE, output = FALSE, fort = TRUE)}

\arguments{
  \item{S}{matrix of available configurations of the response variable (n x TT) with categories starting from 0}
  \item{X1}{matrix of covariates affecting the initial probabilities (n x nc1)}
  \item{X2}{array of covariates affecting the transition probabilities (n x TT-1 x nc2)}
  \item{yv}{vector of frequencies of the available configurations}
  \item{start}{type of starting values (0 = deterministic, 1 = random)}
  \item{tol}{tolerance level for checking convergence of the algorithm}
  \item{maxit}{maximum number of iterations of the algorithm}
  \item{out_se}{to compute the information matrix and standard errors}
  \item{output}{to return additional output (PI,Piv)}
  \item{fort}{to use fortran routine when possible (FALSE for not use fortran)}

}

\value{
  \item{lk }{maximum log-likelihood}
  \item{Be }{estimated array of the parameters affecting the logit for the initial probabilities}
  \item{Ga }{estimated array of the parameters affecting the logit for the transition probabilities}
  \item{np }{number of free parameters}
  \item{aic }{value of AIC for model selection}
  \item{bic }{value of BIC for model selection}
  \item{seBe}{standard errors for Be}
  \item{seGa}{standard errors for Ga}
  \item{Piv }{estimate of initial probability matrix}
  \item{PI }{estimate of transition probability matrices}
  \item{call}{command used to call the function}
}

\references{Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data}, Chapman and Hall/CRC press.}
\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia, http://www.stat.unipg.it/bartolucci}

\examples{

\dontrun{

# Example based on criminal data

# load criminal data
data(data_criminal_sim)

#We consider the response variable referring of crime of type 5

out <- long2wide(data_criminal_sim, "id", "time", "sex",
"y5", aggr = T, full = 999)
XX <- out$XX-1
YY <- out$YY
freq <- out$freq
TT <- 6

X1 <- as.matrix(XX[,1])
X2 <- as.matrix(XX[,2:TT])
# estimate the model
res <- est_mc_cov(S = YY, yv = freq, X1 = X1, X2 = X2, output = TRUE)
summary(res)

# Initial probability for female
Piv0 <- round(colMeans(res$Piv[X1 == 0,]), 4)

# Initial probability for male
Piv1 <- round(colMeans(res$Piv[X1 == 1,]), 4)

}
}
