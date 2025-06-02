\name{search.model.LM}

\alias{search.model.LM}

\title{Search for the global maximum of the log-likelihood}

\description{Function that searches for the global maximum of the log-likelihood of different models given a vector of possible number of states to try for.  \cr \cr \bold{The function is no longer maintained. Please look at \code{\link{lmestSearch}} function}.}

\usage{search.model.LM(version = c("basic","latent","manifest","basic.cont", "latent.cont"),
                kv, ..., nrep = 2, tol1 = 10^-5, tol2 = 10^-10,out_se = FALSE)}

\arguments{
  \item{version }{model to be estimated ("basic" = basic LM model (est_lm_basic function); "latent" = LM model with covariates in the distribution of the latent process (est_lm_cov_latent function); "manifest" = LM model with covariates in the measurement model (est_lm_cov_maifest function),"basic.cont" = basic LM model for continuous outcomes (est_lm_basic_cont function); "latent.cont" = LM model for continuous outcomes with covariates in the distribution of the latent process (est_lm_cov_latent_cont function))}
  \item{kv }{vector of possible number of latent states}
  \item{... }{additional arguments to be passed based on the model to be estimated (see details)}
  \item{nrep }{number of repetitions of each random initialization}
  \item{tol1 }{tolerance level for checking convergence of the algorithm in the random initializations}
  \item{tol2 }{tolerance level for checking convergence of the algorithm in the last deterministic initialization}
  \item{out_se }{TRUE for computing information matrix and standard errors}
}

\details{The function combines deterministic and random initializations strategy to reach the global maximum of the model log-likelihood.
It uses one deterministic initialization (start=0) and a number of random initializations (start=1) proportional to the number of latent states. The tolerance level is set equal to 10^-5. Starting from the best solution obtained in this way, a final run is performed (start=2) with a default tolerance level equal to 10^-10.

Arguments in ... depend on the model to be estimated. They match the arguments to be passed to functions  \code{est_lm_basic}, \code{est_lm_cov_latent}, \code{est_lm_cov_manifest}, \code{est_lm_basic_cont}, or \code{est_lm_cov_latent_cont}.
}

\value{
  \item{out.single }{output of each single model (as from \code{est_lm_basic}, \code{est_lm_cov_latent} or \code{est_lm_cov_manifest}) for each k in kv}
  \item{aicv }{value of AIC index for each k in kv}
  \item{bicv }{value of BIC index for each k in kv}
  \item{lkv}{value of log-likelihood for each k in kv}
}

\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia (IT), http://www.stat.unipg.it/bartolucci}

\examples{
\dontrun{

# example for est_lm_basic
data(data_drug)
data_drug <- as.matrix(data_drug)
S <- data_drug[,1:5]-1
yv <- data_drug[,6]
n <- sum(yv)
# Search Basic LM model

res <- search.model.LM("basic", kv = 1:4, S, yv, mod = 1)
summary(res)

}
}
