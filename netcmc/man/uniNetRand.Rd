\name{uniNetRand}
\alias{uniNetRand}

\title{
  A function that generates samples for a univariate network group model.
}

\description{
  This function generates samples for a univariate network group model, which is
  given by
  
  \deqn{Y_{i_s}|\mu_{i_s} \sim f(y_{i_s}| \mu_{i_s}, \sigma_{e}^{2}) ~~~ i=1,\ldots, N_{s},~s=1,\ldots,S ,}{% 
  Y_{i_s}|\mu_{i_s} \sim f(y_{i_s}| \mu_{i_s}, \sigma_{e}^{2}) ~~~ i=1,\ldots, N_{s},~s=1,\ldots,S ,}
  \deqn{g(\mu_{i_s}) = \boldsymbol{x}^\top_{i_s} \boldsymbol{\beta} + v_{s} + \sum_{j\in \textrm{net}(i_s)}w_{i_sj}u_{j} + w^{*}_{i_s}u^{*},}{% 
  g(\mu_{i_s}) = \boldsymbol{x}^\top_{i_s} \boldsymbol{\beta} + v_{s} + \sum_{j\in \textrm{net}(i_s)}w_{i_sj}u_{j} + w^{*}_{i_s}u^{*},}
  \deqn{\boldsymbol{\beta} \sim \textrm{N}(\boldsymbol{0}, \alpha\boldsymbol{I}),}{% 
  \boldsymbol{\beta} \sim \textrm{N}(\boldsymbol{0}, \alpha\boldsymbol{I}),}
  \deqn{v_{s} \sim \textrm{N}(0, \tau^{2}),}{v_{s} \sim \textrm{N}(0, \tau^{2}),}
  \deqn{u_{j} \sim \textrm{N}(0, \sigma_{u}^{2}),}{u_{j} \sim \textrm{N}(0, \sigma_{u}^{2}),}
  \deqn{u^{*} \sim \textrm{N}(0, \sigma_{u}^{2}),}{u^{*} \sim \textrm{N}(0, \sigma_{u}^{2}),}
  \deqn{\tau^{2} \sim \textrm{Inverse-Gamma}(\alpha_{1}, \xi_{1}),}{\tau^{2} \sim \textrm{Inverse-Gamma}(\alpha_{1}, \xi_{1}),}
  \deqn{\sigma_{u}^{2} \sim \textrm{Inverse-Gamma}(\alpha_{2}, \xi_{2}),}{\sigma_{u}^{2} \sim \textrm{Inverse-Gamma}(\alpha_{2}, \xi_{2}),}
  \deqn{\sigma_{e}^{2} \sim \textrm{Inverse-Gamma}(\alpha_{3}, \xi_{3}).}{% 
  \sigma_{e}^{2} \sim \textrm{Inverse-Gamma}(\alpha_{3}, \xi_{3}).}
  
  The covariates for the \eqn{i}{i}th individual in the \eqn{s}{s}th spatial unit or other grouping are included in a \eqn{p \times 1}{p \times 1} vector \eqn{\boldsymbol{x}_{i_s}}{\boldsymbol{x}_{i_s}}. The corresponding \eqn{p \times 1}{p \times 1} vector of fixed effect parameters are denoted by \eqn{\boldsymbol{\beta}}{\boldsymbol{\beta}}, which has an assumed multivariate Gaussian prior with mean \eqn{\boldsymbol{0}}{\boldsymbol{0}} and diagonal covariance matrix \eqn{\alpha\boldsymbol{I}}{\alpha\boldsymbol{I}} that can be chosen by the user. A conjugate Inverse-Gamma prior is specified for \eqn{\sigma_{e}^{2}}{\sigma_{e}^{2}}, and the corresponding hyperparamaterers (\eqn{\alpha_{3}}{\alpha_{3}}, \eqn{\xi_{3}}{\xi_{3}}) can be chosen by the user.
  
  The \eqn{S \times 1}{S \times 1} vector of random effects for the groups are collectively denoted by  \eqn{\boldsymbol{v} = (v_{1}, \ldots, v_{S})_{S \times 1}}{\boldsymbol{v} = (v_{1}, \ldots, v_{S})_{S \times 1}}, and each element is assigned an independent zero-mean Gaussian prior distribution with a constant variance \eqn{\tau^{2}}{\tau^{2}}.  A conjugate Inverse-Gamma prior is specified for \eqn{\tau^{2}}{\tau^{2}}. The corresponding hyperparamaterers (\eqn{\alpha_{1}}{\alpha_{1}}, \eqn{\xi_{1}}{\xi_{1}}) can be chosen by the user.

  The \eqn{J \times 1}{J \times 1} vector of alter random effects are denoted by \eqn{\boldsymbol{u} = (u_{1}, \ldots, u_{J})_{J \times 1}}{\boldsymbol{u} = (u_{1}, \ldots, u_{J})_{J \times 1}} and modelled as independently Gaussian with mean zero and a constant variance, and due to the row standardised nature of \eqn{\boldsymbol{W}}{\boldsymbol{W}}, \eqn{\sum_{j \in \textrm{net}(i_s)} w_{i_sj} u_{j}}{\sum_{j \in \textrm{net}(i_s)} w_{i_sj} u_{j}} represents the average (mean) effect that the peers of individual \eqn{i}{i} in spatial unit or group \eqn{s}{s} have on that individual. \eqn{w^{*}_{i_s}u^{*}}{w^{*}_{i_s}u^{*}} is an \emph{isolation effect}, which is an effect for individuals who don't nominate any friends. This is achieved by setting \eqn{w^{*}_{i_s}=1}{w^{*}_{i_s}=1} if individual \eqn{i_s}{i_s} nominates no peers and \eqn{w^{*}_{i_s}=0}{w^{*}_{i_s}=0} otherwise, and if  \eqn{w^{*}_{i_s}=1}{w^{*}_{i_s}=1} then clearly \eqn{\sum_{j\in \textrm{net}(i_{s})}w_{i_{s}j}u_{jr}=0}{\sum_{j\in \textrm{net}(i_{s})}w_{i_{s}j}u_{jr}=0} as \eqn{\textrm{net}(i_{s})}{\textrm{net}(i_{s})} is the empty set. A conjugate Inverse-Gamma prior is specified for the random effects variance \eqn{\sigma_{u}^{2}}{\sigma_{u}^{2}}, and the corresponding hyperparamaterers (\eqn{\alpha_{2}}{\alpha_{2}}, \eqn{\xi_{2}}{\xi_{2}}) can be chosen by the user.
   
  The exact specification of each of the likelihoods (binomial, Gaussian, and Poisson) are given below:
  
  \deqn{\textrm{Binomial:} ~ Y_{i_s} \sim \textrm{Binomial}(n_{i_s}, \theta_{i_s}) ~ \textrm{and} ~ g(\mu_{i_s}) = \textrm{ln}(\theta_{i_s} / (1 - \theta_{i_s})),}{\textrm{Binomial:} ~ Y_{i_s} \sim \textrm{Binomial}(n_{i_s}, \theta_{i_s}) ~ \textrm{and} ~ g(\mu_{i_s}) = \textrm{ln}(\theta_{i_s} / (1 - \theta_{i_s})),}
  \deqn{\textrm{Gaussian:} ~ Y_{i_s} \sim \textrm{N}(\mu_{i_s}, \sigma_{e}^{2}) ~ \textrm{and} ~ g(\mu_{i_s}) = \mu_{i_s},}{\textrm{Gaussian:} ~ Y_{i_s} \sim \textrm{N}(\mu_{i_s}, \sigma_{e}^{2}) ~ \textrm{and} ~ g(\mu_{i_s}) = \mu_{i_s},}
  \deqn{\textrm{Poisson:} ~ Y_{i_s} \sim \textrm{Poisson}(\mu_{i_s}) ~ \textrm{and} ~ g(\mu_{i_s}) = \textrm{ln}(\mu_{i_s}).}{\textrm{Poisson:} ~ Y_{i_s} \sim \textrm{Poisson}(\mu_{i_s}) ~ \textrm{and} ~ g(\mu_{i_s}) = \textrm{ln}(\mu_{i_s}).}
}

\usage{
uniNetRand(formula, data, trials, family, groupAssignment, W, numberOfSamples = 10, 
burnin = 0, thin = 1, seed = 1, trueBeta = NULL, 
trueGroupRandomEffects = NULL, trueURandomEffects = NULL, 
trueTauSquared = NULL, trueSigmaSquaredU = NULL, 
trueSigmaSquaredE = NULL, covarianceBetaPrior = 10^5, a1 = 0.001, b1 = 0.001, 
a2 = 0.001, b2 = 0.001, a3 = 0.001, b3 = 0.001, 
centerGroupRandomEffects = TRUE, centerURandomEffects = TRUE)
}

\arguments{

  \item{formula}{
    A formula for the covariate part of the model using a similar 
    syntax to that used in the lm() function.
  }
  \item{data}{
    An optional data.frame containing the variables in the formula.
  }
  \item{trials}{
    A vector the same length as the response containing the total number of trials 
    \eqn{n_{i_s}}{n_{i_s}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``binomial".
  }
  \item{family}{
    The data likelihood model that must be ``gaussian", ``poisson" or ``binomial".
  }
  \item{W}{
    A matrix \eqn{\boldsymbol{W}}{\boldsymbol{W}} that encodes the social network structure and whose rows sum to 1.
  }
  \item{groupAssignment}{
    The binary matrix of individual's assignment to groups used in the model fitting process.
  }
  \item{numberOfSamples}{
    The number of samples to generate pre-thin.
  }
  \item{burnin}{
    The number of MCMC samples to discard as the burn-in period.
  }
  \item{thin}{
    The value by which to thin \eqn{\texttt{numberOfSamples}}{\texttt{numberOfSamples}}.
  }
  \item{seed}{
    A seed for the MCMC algorithm.
  }
  \item{trueBeta}{
    If available, the true value of \eqn{\boldsymbol{\beta}}{\boldsymbol{\beta}}.
  }
  \item{trueGroupRandomEffects}{
    If available, the true value of \eqn{\boldsymbol{v}}{\boldsymbol{v}}.
  }
  \item{trueURandomEffects}{
    If available, the true value of \eqn{\boldsymbol{u}}{\boldsymbol{u}}.
  }
  \item{trueTauSquared}{
    If available, the true value \eqn{\tau^{2}}{\tau^{2}}.
  }
  \item{trueSigmaSquaredU}{
    If available, the true value \eqn{\sigma_{u}^{2}}{\sigma_{u}^{2}}.
  }
  \item{trueSigmaSquaredE}{
    If available, the true value \eqn{\sigma_{e}^{2}}{\sigma_{e}^{2}}.
  }
  \item{covarianceBetaPrior}{
    A scalar prior \eqn{\alpha}{\alpha} for the covariance parameter of the 
    beta prior, such that the covariance is \eqn{\alpha\boldsymbol{I}}{\alpha\boldsymbol{I}}.
  }
  \item{a1}{
    The shape parameter for the Inverse-Gamma distribution 
    relating to the group random effects \eqn{\alpha_{1}}{\alpha_{1}}. 
  }
  \item{b1}{
    The shape parameter for the Inverse-Gamma distribution 
    relating to the group random effects \eqn{\xi_{1}}{\xi_{1}}. 
  }
  \item{a2}{
    The shape parameter for the Inverse-Gamma distribution 
    relating to the network random effects \eqn{\alpha_{2}}{\alpha_{2}}.
  }
  \item{b2}{
    The scale parameter for the Inverse-Gamma distribution 
    relating to the network random effects \eqn{\xi_{2}}{\xi_{2}}. 
  }
  \item{a3}{
    The shape parameter for the Inverse-Gamma distribution 
    relating to the error terms \eqn{\alpha_{3}}{\alpha_{3}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``gaussian".
  }
  \item{b3}{
    The scale parameter for the Inverse-Gamma distribution 
    relating to the error terms \eqn{\xi_{3}}{\xi_{3}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``gaussian".
  }
  \item{centerGroupRandomEffects}{
    A choice to center the group random effects after each 
    iteration of the MCMC sampler.
  }
  \item{centerURandomEffects}{
     A choice to center the network random effects after each 
    iteration of the MCMC sampler.
  }
}

\value{

  \item{call}{
    The matched call.
  }
  \item{y}{
    The response used.
  }
  \item{X}{
    The design matrix used.
  }
  \item{standardizedX}{
    The standardized design matrix used.
  }
  \item{groupAssignment}{
    The group assignment matrix used.
  }
  \item{W}{
    The network matrix used.
  }
  \item{samples}{
    The matrix of simulated samples from the posterior
    distribution of each parameter in the model (excluding random effects).
  }
  \item{betaSamples}{
    The matrix of simulated samples from the posterior 
    distribution of \eqn{\boldsymbol{\beta}}{\boldsymbol{\beta}} parameters in the model.
  }
  \item{tauSquaredSamples}{
    The vector of simulated samples from the posterior 
    distribution of \eqn{\tau^{2}}{\tau^{2}} in the model.
  }
  \item{sigmaSquaredUSamples}{
    The vector of simulated samples from the posterior 
    distribution of \eqn{\sigma_{u}^{2}}{\sigma_{u}^{2}} in the model.
  }
  \item{sigmaSquaredESamples}{
    The vector of simulated samples from the posterior 
    distribution of \eqn{\sigma_{e}^{2}}{\sigma_{e}^{2}} in the model.
  }
  \item{groupRandomEffectsSamples}{
    The matrix of simulated samples from the posterior 
    distribution of spatial/grouping random effects \eqn{\boldsymbol{v}}{\boldsymbol{v}} in the model.
  }
  \item{uRandomEffectsSamples}{
    The matrix of simulated samples from the posterior 
    distribution of network random effects \eqn{\boldsymbol{u}}{\boldsymbol{u}} in the model.
  }
  \item{acceptanceRates}{
    The acceptance rates of parameters in the model (excluding 
    random effects) from the MCMC sampling scheme .
  }
  \item{groupRandomEffectsAcceptanceRate}{
    The acceptance rates of spatial/grouping random effects in the
    model from the MCMC sampling scheme.
  }
  \item{uRandomEffectsAcceptanceRate}{
    The acceptance rates of network random effects in the model 
    from the MCMC sampling scheme.
  }
  \item{timeTaken}{
    The time taken for the model to run.
  }
  \item{burnin}{
    The number of MCMC samples to discard as the burn-in period.
  }
  \item{thin}{
    The value by which to thin \eqn{\texttt{numberOfSamples}}{\texttt{numberOfSamples}}.
  }
  \item{DBar}{
    DBar for the model.
  }
  \item{posteriorDeviance}{
    The posterior deviance for the model.
  }
  \item{posteriorLogLikelihood}{
    The posterior log likelihood for the model.
  }
  \item{pd}{
    The number of effective parameters in the model.
  }
  \item{DIC}{
    The DIC for the model.
  }
  
}

\author{
George Gerogiannis
}

\examples{
  #################################################
  #### Run the model on simulated data
  #################################################
  #### Load other libraries required
  library(MCMCpack)
  
  #### Set up a network
  observations <- 200
  numberOfMultipleClassifications <- 50
  W <- matrix(rbinom(observations * numberOfMultipleClassifications, 1, 0.05), 
              ncol = numberOfMultipleClassifications)
  numberOfActorsWithNoPeers <- sum(apply(W, 1, function(x) { sum(x) == 0 }))
  peers <- sample(1:numberOfMultipleClassifications, numberOfActorsWithNoPeers, 
                  TRUE)
  actorsWithNoPeers <- which(apply(W, 1, function(x) { sum(x) == 0 }))
  for(i in 1:numberOfActorsWithNoPeers) {
    W[actorsWithNoPeers[i], peers[i]] <- 1
  }
  W <- t(apply(W, 1, function(x) { x / sum(x) }))
  
  #### Set up a single level classification
  numberOfSingleClassifications <- 20
  factor = sample(1:numberOfSingleClassifications, observations, TRUE)
  V = matrix(NA, ncol = numberOfSingleClassifications, nrow = observations)
  for(i in 1:length(factor)){
    for(j in 1:numberOfSingleClassifications){
      if(factor[i] == j){
        V[i, j] = 1
      } else {
        V[i, j] = 0
      }
    }
  }
  
  #### Generate the covariates and response data
  X <- matrix(rnorm(2 * observations), ncol = 2)
  colnames(X) <- c("x1", "x2")
  beta <- c(1, -0.5, 0.5)
  tauSquared <- 0.5
  vRandomEffects <- rnorm(numberOfSingleClassifications, mean = 0, 
                          sd = sqrt(tauSquared))
  sigmaSquaredU <- 1
  uRandomEffects <- rnorm(numberOfMultipleClassifications, mean = 0, 
                          sd = sqrt(sigmaSquaredU))

  logTheta <- cbind(rep(1, observations), X) \%*\% beta + V \%*\% vRandomEffects 
            + W \%*\% uRandomEffects
  Y <- rpois(n = observations, lambda = exp(logTheta))
  data <- data.frame(cbind(Y, X))
  
  #### Run the model
  formula <- Y ~ x1 + x2
  \dontrun{model <- uniNetRand(formula = formula, data = data, family="poisson", 
                               W = W, groupAssignment = V, 
                               numberOfSamples = 10000, burnin = 10000,
                               thin = 10, seed = 1)}

}
