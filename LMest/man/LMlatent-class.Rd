\name{LMlatent-class}
\alias{LMlatent-class}
\docType{class}

\title{Class \code{'LMlatent'}}

\description{An S3 class object created by \code{\link{lmest}} for Latent Markov (LM) model with covariates in the latent model.}

\value{
 \item{lk}{maximum log-likelihood}
  \item{Be}{estimated array of the parameters affecting the logit for the initial probabilities}
  \item{Ga}{estimated array of the parameters affecting the logit for the transition probabilities}
  \item{Piv}{estimate of initial probability matrix. The first state is used as reference category when \code{param = "multilogit"}}
  \item{PI}{estimate of transition probability matrices. State u is used as reference category when \code{paramLatent = "multilogit"}}
  \item{Psi}{estimate of conditional response probabilities (mb x k x r)}
   \item{np}{number of free parameters}
     \item{k}{optimal number of latent states}
  \item{aic}{value of the Akaike Information Criterion for model selection}
  \item{bic}{value of the Bayesian Information Criterion for model selection}
  \item{lkv}{log-likelihood trace at every step of the EM algorithm}
    \item{n}{number of observations in the data}
  \item{TT}{number of time occasions}
  \item{paramLatent}{type of parametrization for the transition probabilities ("multilogit" = standard multinomial logit for every row of the transition matrix, "difflogit" = multinomial logit based on the difference between two sets of parameters)}
  \item{sePsi}{standard errors for the conditional response matrix}
  \item{seBe}{standard errors for \code{Be}}
  \item{seGa}{standard errors for \code{Ga}}
     \item{Lk}{vector containing the values of the log-likelihood of the LM model with each \code{k} (latent states)}
  \item{Bic}{vector containing the values of the BIC for each \code{k}}
  \item{Aic}{vector containing the values of the AIC for each \code{k}}
  \item{V}{array containing the posterior distribution of the latent states for each response configuration and time occasion}
  \item{Ul}{matrix containing the predicted sequence of latent states by the local decoding method}
   	\item{S}{array containing the available response configurations}
 	  \item{yv}{vector of frequencies of the available configurations}
 	\item{Pmarg}{matrix containing the marginal distribution of the latent states} 
  \item{call}{command used to call the function}
  \item{data}{Data frame given in input}
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}


\seealso{\code{\link{lmest}}}


