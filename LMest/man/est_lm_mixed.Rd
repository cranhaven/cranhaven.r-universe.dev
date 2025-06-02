\name{est_lm_mixed}

\alias{est_lm_mixed}

\title{Estimate mixed LM model}

\description{Main function for estimating the mixed LM model with discrete random effect in the latent model. \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{lmestMixed}} \bold{function}}

\usage{est_lm_mixed(S, yv = rep(1,nrow(S)), k1, k2, start = 0, tol = 10^-8, maxit = 1000,
                    out_se = FALSE)}

\arguments{
  \item{S}{array of available response configurations (n x TT x r) with categories starting from 0}
  \item{yv}{vector of frequencies of the available configurations}
  \item{k1}{number of latent classes}
  \item{k2}{number of latent states}
  \item{start}{type of starting values (0 = deterministic, 1 = random)}
  \item{tol}{tolerance level for convergence}
   \item{maxit}{maximum number of iterations of the algorithm}
  \item{out_se}{to compute standard errors}
}

\value{
  \item{la }{estimate of the mass probability vector (distribution of the random effects)}
  \item{Piv }{estimate of initial probabilities}
  \item{Pi }{estimate of transition probability matrices}
  \item{Psi }{estimate of conditional response probabilities}
  \item{lk }{maximum log-likelihood}
  \item{W }{posterior probabilities of the random effect}
  \item{np }{number of free parameters}
  \item{bic }{value of BIC for model selection}
  \item{call}{command used to call the function}
}

\references{Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data},
	Chapman and Hall/CRC press.}
\author{Francesco Bartolucci, Silvia Pandolfi - University of Perugia (IT)}

\examples{
\dontrun{
# Example based of criminal data

# load data
data(data_criminal_sim)
out <- long2wide(data_criminal_sim, "id", "time", "sex",
	 c("y1","y2","y3","y4","y5","y6","y7","y8","y9","y10"), aggr = T, full = 999)

XX <- out$XX
YY <- out$YY
freq <- out$freq
n1 <- sum(freq[XX[,1] == 1])
n2 <- sum(freq[XX[,1] == 2])
n <- sum(freq)

# fit mixed LM model only for females
YY <- YY[XX[,1] == 2,,]
freq <- freq[XX[,1] == 2]
k1 <- 2
k2 <- 2
res <- est_lm_mixed(YY, freq, k1, k2, tol = 10^-8)
summary(res)
}
}
