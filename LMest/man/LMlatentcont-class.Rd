\name{LMlatentcont-class}
\alias{LMlatentcont-class}
\docType{class}

\title{Class \code{'LMlatentcont'}}

\description{An S3 class object created by \code{\link{lmestCont}} for the Latent Markov (LM) model for continuous responses in long format with covariates in the latent model.}

\value{
 \item{lk}{maximum log-likelihood}
 \item{Be}{estimated array of the parameters affecting the logit for the initial probabilities}
  \item{Ga}{estimated array of the parameters affecting the logit for the transition probabilities}
  \item{Mu}{estimate of conditional means of the response variables}
  \item{Si}{estimate of var-cov matrix common to all states}
  \item{np}{number of free parameters}
    \item{k}{optimal number of latent states}
  \item{aic}{value of the Akaike Information Criterion for model selection}
  \item{bic}{value of the Bayesian Information Criterion for model selection}
 \item{lkv}{log-likelihood trace at every step}
   \item{n}{number of observations in the data}
  \item{TT}{number of time occasions}
  \item{paramLatent}{type of parametrization for the transition probabilities ("multilogit" = standard multinomial logit for every row of the transition matrix, "difflogit" = multinomial logit based on the difference between two sets of parameters)}
   \item{seMu}{standard errors for the conditional means}
   \item{seSi}{standard errors for the var-cov matrix}
  \item{seBe}{standard errors for \code{Be}}
  \item{seGa}{standard errors for Ga}
  \item{sc}{score vector}
  \item{J}{information matrix}
  \item{PI}{estimate of transition probability matrices}
    \item{Piv}{estimate of initial probability matrix}
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
  Alessio Serafini
}


\seealso{\code{\link{lmestCont}}}


