\name{FFBS_BinomialNormal}
\alias{FFBS_BinomialNormal}

\title{
  
  Posterior (MCMC) samples for a hidden Markov model with a Binomial and Normal response using the forward-filtering backward-sampling algorithm.
  
}

\description{
    Posterior (MCMC) samples for a hidden Markov model with a Binomial and Normal response using the forward-filtering backward-sampling algorithm.
}

\usage{
  
  FFBS_BinomialNormal(bin,norm,nstates,hyperPars=list(),ntimes,niter=1000,nburnin=0)
  
}

\arguments{
  
  \item{bin}{the Binomial response variable. As in the glm function, this can be a binary vector with 1 indicating success and 0 failure, a factor where the first level is considered a failure and all other leves success, or a matrix with the number of successes and failures in the columns.}
  
  \item{norm}{the Normal response variable. This should be a numeric vector.}
  
  \item{nstates}{the required number of states in the hidden Markov model.}
  
  \item{hyperPars}{a named \code{list} with values of the hyper-parameters. See details.}
  
  \item{ntimes}{the lengths of time series in arguments \code{bin} and \code{norm}; it
    defaults to assuming a single time series of length \code{length(bin)}.}
  
  \item{niter}{number of iterations to run the sampler for.}
  
  \item{nburnin}{number of initial samples to discard as burnin,}
  
}

\details{
  
  This function runs the forward-filtering backwards-sampling MCMC algorithm for a hidden Markov model with a Binomial and Normal response variable. The response variables are assumed conditionally independent given the states.
  
  The following conjugate prior distributions are used:
  
  For the initial state probabilities, a Dirichlet prior with parameter vector \code{init_alpha}
  
  For each row in the transition probability matrix, a Dirichlet prior is used. The parameters of these Dirichlet distributions are contained in the matrix \code{trans_alpha}.
  
  For the probability of correct in the Binomial response, a Beta prior is used, with parameters \code{bin_alpha} and \code{bin_beta}.
  
  For the mean and variance of the Normal response, a Normal-inverse-Gamma prior is used.
  
  This function was written mainly for didactive purposes, not for speed (or compatibility with other packages which provide posterior samples). 
  
}

\value{

  A named list with samples of the different parameters.
}

\references{
  
  Visser, I., & Speekenbrink, M. (in preparation). Mixture and hidden Markov models in R.
  
}

\author{
  Maarten Speekenbrink
}


\examples{

\dontrun{
  data(speed)
  set.seed(1)
  hyperPars <- list(norm_invsigma_scale=.01,norm_invsigma_shape=.01,norm_mu_sca=.1)
  mcmc_samples <- FFBS_BinomialNormal(speed$corr,speed$rt,nstates=2,
        ntimes=c(168,134,137),niter=500,hyperPars = hyperPars)
        
  plot(mcmc_samples$mu[,1])
  hist(mcmc_samples$mu[,1])
}

}

\keyword{model}
\keyword{statistics}% __ONLY ONE__ keyword per line
