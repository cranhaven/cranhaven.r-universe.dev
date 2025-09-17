\name{uni}
\alias{uni}

\title{
  A function that generates samples for a univariate fixed effects model.
}

\description{
  This function generates samples for a univariate fixed effects model, which is
  given by
  
  \deqn{Y_{i_s}|\mu_{i_s} \sim f(y_{i_s}| \mu_{i_s}, \sigma_{e}^{2}) ~~~ i=1,\ldots, N_{s},~s=1,\ldots,S ,}{% 
  Y_{i_s}|\mu_{i_s} \sim f(y_{i_s}| \mu_{i_s}, \sigma_{e}^{2}) ~~~ i=1,\ldots, N_{s},~s=1,\ldots,S ,}
  \deqn{g(\mu_{i_s}) = \boldsymbol{x}^\top_{i_s} \boldsymbol{\beta},}{% 
  g(\mu_{i_s}) = \boldsymbol{x}^\top_{i_s} \boldsymbol{\beta},}
  \deqn{\boldsymbol{\beta} \sim \textrm{N}(\boldsymbol{0}, \alpha\boldsymbol{I}),}{% 
  \boldsymbol{\beta} \sim \textrm{N}(\boldsymbol{0}, \alpha\boldsymbol{I}),}
  \deqn{\sigma_{e}^{2} \sim \textrm{Inverse-Gamma}(\alpha_{3}, \xi_{3}).}{% 
  \sigma_{e}^{2} \sim \textrm{Inverse-Gamma}(\alpha_{3}, \xi_{3}).}
  
  The covariates for the \eqn{i}{i}th individual in the \eqn{s}{s}th spatial unit or other grouping are included in a \eqn{p \times 1}{p \times 1} vector \eqn{\boldsymbol{x}_{i_s}}{\boldsymbol{x}_{i_s}}. The corresponding \eqn{p \times 1}{p \times 1} vector of fixed effect parameters are denoted by \eqn{\boldsymbol{\beta}}{\boldsymbol{\beta}}, which has an assumed multivariate Gaussian prior with mean \eqn{\boldsymbol{0}}{\boldsymbol{0}} and diagonal covariance matrix \eqn{\alpha\boldsymbol{I}}{\alpha\boldsymbol{I}} that can be chosen by the user. A conjugate Inverse-Gamma prior is specified for \eqn{\sigma_{e}^{2}}{\sigma_{e}^{2}}, and the corresponding hyperparamaterers (\eqn{\alpha_{3}}{\alpha_{3}}, \eqn{\xi_{3}}{\xi_{3}}) can be chosen by the user.
   
  The exact specification of each of the likelihoods (binomial, Gaussian, and Poisson) are given below:
  
  \deqn{\textrm{Binomial:} ~ Y_{i_s} \sim \textrm{Binomial}(n_{i_s}, \theta_{i_s}) ~ \textrm{and} ~ g(\mu_{i_s}) = \textrm{ln}(\theta_{i_s} / (1 - \theta_{i_s})),}{\textrm{Binomial:} ~ Y_{i_s} \sim \textrm{Binomial}(n_{i_s}, \theta_{i_s}) ~ \textrm{and} ~ g(\mu_{i_s}) = \textrm{ln}(\theta_{i_s} / (1 - \theta_{i_s})),}
  \deqn{\textrm{Gaussian:} ~ Y_{i_s} \sim \textrm{N}(\mu_{i_s}, \sigma_{e}^{2}) ~ \textrm{and} ~ g(\mu_{i_s}) = \mu_{i_s},}{\textrm{Gaussian:} ~ Y_{i_s} \sim \textrm{N}(\mu_{i_s}, \sigma_{e}^{2}) ~ \textrm{and} ~ g(\mu_{i_s}) = \mu_{i_s},}
  \deqn{\textrm{Poisson:} ~ Y_{i_s} \sim \textrm{Poisson}(\mu_{i_s}) ~ \textrm{and} ~ g(\mu_{i_s}) = \textrm{ln}(\mu_{i_s}).}{\textrm{Poisson:} ~ Y_{i_s} \sim \textrm{Poisson}(\mu_{i_s}) ~ \textrm{and} ~ g(\mu_{i_s}) = \textrm{ln}(\mu_{i_s}).}
  
  
}

\usage{
uni(formula, data, trials, family, numberOfSamples = 10, burnin = 0, thin = 1, seed = 1,
trueBeta = NULL, trueSigmaSquaredE = NULL, covarianceBetaPrior = 10^5, 
a3 = 0.001, b3 = 0.001)
}

\arguments{

  \item{formula}{
    A formula for the covariate part of the model using a similar 
    syntax to that used in the lm() function.
  }
  \item{data}{
    An optional data.frame containing the  variables in the formula.
  }
  \item{trials}{
    A vector the same length as the response containing the total number of trials 
    \eqn{n_{i_s}}{n_{i_s}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``binomial".
  }
  \item{family}{
    The data likelihood model that must be ``gaussian"
    , ``poisson" or ``binomial".
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
    If available, the true values of the \eqn{\boldsymbol{\beta}}{\boldsymbol{\beta}}.
  }
  \item{trueSigmaSquaredE}{
    If available, the true value of \eqn{\sigma_{e}^{2}}{\sigma_{e}^{2}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``gaussian".
  }
  \item{covarianceBetaPrior}{
    A scalar prior \eqn{\alpha}{\alpha} for the covariance parameter of the 
    beta prior, such that the covariance is \eqn{\alpha\boldsymbol{I}}{\alpha\boldsymbol{I}}.
  }
  \item{a3}{
    The shape parameter for the Inverse-Gamma distribution 
    \eqn{\alpha_{3}}{\alpha_{3}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``gaussian".
  }
  \item{b3}{
    The scale parameter for the Inverse-Gamma distribution 
    \eqn{\xi_{3}}{\xi_{3}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``gaussian".
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
  \item{samples}{
    The matrix of simulated samples from the posterior
    distribution of each parameter in the model (excluding random effects).
  }
  \item{betaSamples}{
    The matrix of simulated samples from the posterior 
    distribution of \eqn{\boldsymbol{\beta}}{\boldsymbol{\beta}} parameters in the model.
  }
  \item{sigmaSquaredESamples}{
    The vector of simulated samples from the posterior 
    distribution of \eqn{\sigma_{e}^{2}}{\sigma_{e}^{2}} in the model.
  }
  \item{acceptanceRates}{
    The acceptance rates of parameters in the model from the MCMC 
    sampling scheme.
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
  
  #### Generate the covariates and response data
  observations <- 100
  X <- matrix(rnorm(2 * observations), ncol = 2)
  colnames(X) <- c("x1", "x2")
  beta <- c(2, -2, 2)
  logit <- cbind(rep(1, observations), X) \%*\% beta
  prob <- exp(logit) / (1 + exp(logit))
  trials <- rep(50, observations)
  Y <- rbinom(n = observations, size = trials, prob = prob)
  data <- data.frame(cbind(Y, X))
  
  #### Run the model
  formula <- Y ~ x1 + x2
  \dontrun{model <- uni(formula = formula, data = data, family="binomial", 
                        trials = trials, numberOfSamples = 10000, 
                        burnin = 10000, thin = 10, seed = 1)}
                        
}
