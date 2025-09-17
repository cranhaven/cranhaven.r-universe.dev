\name{multiNet}
\alias{multiNet}

\title{
  A function that generates samples for a multivariate fixed effects and network
  model.
}

\description{
  This function that generates samples for a multivariate fixed effects and network
  model, which is given by
  
  \deqn{Y_{i_sr}|\mu_{i_sr} \sim f(y_{i_sr}| \mu_{i_sr}, \sigma_{er}^{2}) ~~~ i=1,\ldots, N_{s},~s=1,\ldots,S ,~r=1,\ldots,R,}{Y_{i_sr}|\mu_{i_sr} \sim f(y_{i_sr}| \mu_{i_sr}, \sigma_{er}^{2}) ~~~ i=1,\ldots, N_{s},~s=1,\ldots,S ,~r=1,\ldots,R,}
  \deqn{g(\mu_{i_sr}) = \boldsymbol{x}^\top_{i_s} \boldsymbol{\beta}_{r} + \sum_{j\in \textrm{net}(i_s)}w_{i_sj}u_{jr}+ w^{*}_{i_s}u^{*}_{r},}{g(\mu_{i_sr}) = \boldsymbol{x}^\top_{i_s} \boldsymbol{\beta}_{r} + \sum_{j\in \textrm{net}(i_s)}w_{i_sj}u_{jr}+ w^{*}_{i_s}u^{*}_{r},}
  \deqn{\boldsymbol{\beta}_{r} \sim \textrm{N}(\boldsymbol{0}, \alpha\boldsymbol{I})}{\boldsymbol{\beta}_{r} \sim \textrm{N}(\boldsymbol{0}, \alpha\boldsymbol{I})}
  \deqn{\boldsymbol{u}_{j} = (u_{1j},\ldots, u_{Rj}) \sim \textrm{N}(\boldsymbol{0}, \boldsymbol{\Sigma}_{\boldsymbol{u}}),}{\boldsymbol{u}_{j} = (u_{1j},\ldots, u_{Rj}) \sim \textrm{N}(\boldsymbol{0}, \boldsymbol{\Sigma}_{\boldsymbol{u}}),}
  \deqn{\boldsymbol{u}^{*} = (u_{1}^*,\ldots, u_{R}^*) \sim \textrm{N}(\boldsymbol{0}, \boldsymbol{\Sigma}_{\boldsymbol{u}}),}{\boldsymbol{u}^{*} = (u_{1}^*,\ldots, u_{R}^*) \sim \textrm{N}(\boldsymbol{0}, \boldsymbol{\Sigma}_{\boldsymbol{u}}),}
  \deqn{\boldsymbol{\Sigma}_{\boldsymbol{u}} \sim \textrm{Inverse-Wishart}(\xi_{\boldsymbol{u}}, \boldsymbol{\Omega}_{\boldsymbol{u}}),}{\boldsymbol{\Sigma}_{\boldsymbol{u}} \sim \textrm{Inverse-Wishart}(\xi_{\boldsymbol{u}}, \boldsymbol{\Omega}_{\boldsymbol{u}}),}
  \deqn{\sigma_{er}^{2} \sim \textrm{Inverse-Gamma}(\alpha_{3}, \xi_{3}).}{% 
  \sigma_{er}^{2} \sim \textrm{Inverse-Gamma}(\alpha_{3}, \xi_{3}).}
  
  The covariates for the \eqn{i}{i}th individual in the \eqn{s}{s}th spatial unit or other grouping are included in a \eqn{p \times 1}{p \times 1} vector \eqn{\boldsymbol{x}_{i_s}}{\boldsymbol{x}_{i_s}}. The corresponding \eqn{p \times 1}{p \times 1} vector of fixed effect parameters relating to the \eqn{r}{r}th response are denoted by \eqn{\boldsymbol{\beta}_{r}}{\boldsymbol{\beta}_{r}}, which has an assumed multivariate Gaussian prior with mean \eqn{\boldsymbol{0}}{\boldsymbol{0}} and diagonal covariance matrix \eqn{\alpha\boldsymbol{I}}{\alpha\boldsymbol{I}} that can be chosen by the user. A conjugate Inverse-Gamma prior is specified for \eqn{\sigma_{er}^{2}}{\sigma_{er}^{2}}, and the corresponding hyperparamaterers (\eqn{\alpha_{3}}{\alpha_{3}}, \eqn{\xi_{3}}{\xi_{3}}) can be chosen by the user.

The \eqn{R \times 1}{R \times 1} vector of random effects for the \eqn{j}{j}th alter is denoted by \eqn{\boldsymbol{u}_{j} = (u_{j1}, \ldots, u_{jR})_{R \times 1}}{\boldsymbol{u}_{j} = (u_{j1}, \ldots, u_{jR})_{R \times 1}}, while the \eqn{R \times 1}{R \times 1} vector of isolation effects for all \eqn{R}{R} outcomes is  denoted by \eqn{\boldsymbol{u}^{*} = (u_{1}^*,\ldots, u_{R}^*)}{\boldsymbol{u}^{*} = (u_{1}^*,\ldots, u_{R}^*)}, and both are assigned multivariate Gaussian prior distributions. The unstructured covariance matrix \eqn{\boldsymbol{\Sigma}_{\boldsymbol{u}}}{\boldsymbol{\Sigma}_{\boldsymbol{u}}} captures the covariance between the \eqn{R}{R} outcomes at the network level, and a conjugate Inverse-Wishart prior is specified for this covariance matrix \eqn{\boldsymbol{\Sigma}_{\boldsymbol{u}}}{\boldsymbol{\Sigma}_{\boldsymbol{u}}}. The corresponding hyperparamaterers (\eqn{\xi_{\boldsymbol{u}}}{\xi_{\boldsymbol{u}}}, \eqn{\boldsymbol{\Omega}_{\boldsymbol{u}}}{\boldsymbol{\Omega}_{\boldsymbol{u}}}) can be chosen by the user.

  The exact specification of each of the likelihoods (binomial, Gaussian, and Poisson) are given below:
  
  \deqn{\textrm{Binomial:} ~ Y_{i_sr} \sim \textrm{Binomial}(n_{i_sr}, \theta_{i_sr}) ~ \textrm{and} ~ g(\mu_{i_sr}) = \textrm{ln}(\theta_{i_sr} / (1 - \theta_{i_sr})),}{\textrm{Binomial:} ~ Y_{i_sr} \sim \textrm{Binomial}(n_{i_s}, \theta_{i_sr}) ~ \textrm{and} ~ g(\mu_{i_sr}) = \textrm{ln}(\theta_{i_sr} / (1 - \theta_{i_sr})),}
  \deqn{\textrm{Gaussian:} ~ Y_{i_sr} \sim \textrm{N}(\mu_{i_sr}, \sigma_{er}^{2}) ~ \textrm{and} ~ g(\mu_{i_sr}) = \mu_{i_sr},}{\textrm{Gaussian:} ~ Y_{i_sr} \sim \textrm{N}(\mu_{i_sr}, \sigma_{er}^{2}) ~ \textrm{and} ~ g(\mu_{i_sr}) = \mu_{i_sr},}
  \deqn{\textrm{Poisson:} ~ Y_{i_sr} \sim \textrm{Poisson}(\mu_{i_sr}) ~ \textrm{and} ~ g(\mu_{i_sr}) = \textrm{ln}(\mu_{i_sr}).}{\textrm{Poisson:} ~ Y_{i_sr} \sim \textrm{Poisson}(\mu_{i_sr}) ~ \textrm{and} ~ g(\mu_{i_sr}) = \textrm{ln}(\mu_{i_sr}).}
}

\usage{
multiNet(formula, data, trials, family, W, numberOfSamples = 10, burnin = 0, 
thin = 1, seed = 1, trueBeta = NULL, trueURandomEffects = NULL, 
trueVarianceCovarianceU = NULL, trueSigmaSquaredE = NULL, 
covarianceBetaPrior = 10^5, xi, omega, a3 = 0.001, b3 = 0.001, 
centerURandomEffects = TRUE)
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
    \eqn{n_{i_sr}}{n_{i_sr}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``binomial".
  }
  \item{family}{
    The data likelihood model that must be ``gaussian", ``poisson" or 
    ``binomial".
  }
  \item{W}{
    A matrix \eqn{\boldsymbol{W}}{\boldsymbol{W}} that encodes the social network structure and whose rows sum to 1.
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
    If available, the true value of \eqn{\boldsymbol{\beta}_{1}, \ldots, \boldsymbol{\beta}_{R}}{\boldsymbol{\beta}_{1}, \ldots, \boldsymbol{\beta}_{R}}.
  }
  \item{trueURandomEffects}{
    If available, the true values of \eqn{\boldsymbol{u}_{1}, \ldots, \boldsymbol{u}_{J}, \boldsymbol{u}^{*}}{\boldsymbol{u}_{1}, \ldots, \boldsymbol{u}_{J}, \boldsymbol{u}^{*}}.
  }
  \item{trueVarianceCovarianceU}{
    If available, the true value of \eqn{\boldsymbol{\Sigma}_{\boldsymbol{u}}}{\boldsymbol{\Sigma}_{\boldsymbol{u}}}.
  }
  \item{trueSigmaSquaredE}{
    If available, the true value of \eqn{\sigma_{e1}^{2}}{\sigma_{e1}^{2}}, \eqn{\ldots}{\ldots},  \eqn{\sigma_{eR}^{2}}{\sigma_{eR}^{2}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``gaussian".
  }
  \item{covarianceBetaPrior}{
    A scalar prior \eqn{\alpha}{\alpha} for the covariance parameter of the 
    beta prior, such that the covariance is \eqn{\alpha\boldsymbol{I}}{\alpha\boldsymbol{I}}.
  }
  \item{xi}{
    The degrees of freedom parameter for the Inverse-Wishart
    distribution relating to the network random effects \eqn{\xi_{\boldsymbol{u}}}{\xi_{\boldsymbol{u}}}. 
  }
  \item{omega}{
    The scale parameter for the Inverse-Wishart distribution 
    relating to the network random effects \eqn{\boldsymbol{\Omega}_{\boldsymbol{u}}}{\boldsymbol{\Omega}_{\boldsymbol{u}}}.  
  }
  \item{a3}{
    The shape parameter for the Inverse-Gamma distribution 
    relating to the error terms \eqn{\alpha_{3}}{\alpha_{3}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``gaussian".
  }
  \item{b3}{
    The scale parameter for the Inverse-Gamma distribution 
    relating to the error terms \eqn{\xi_{3}}{\xi_{3}}. Only used if \eqn{\texttt{family}}{\texttt{family}}=``gaussian".
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
  \item{W}{
    The network matrix used.
  }
  \item{samples}{
    The matrix of simulated samples from the posterior
    distribution of each parameter in the model (excluding random effects).
  }
  \item{betaSamples}{
    The matrix of simulated samples from the posterior 
    distribution of \eqn{\boldsymbol{\beta}_{1}, \ldots, \boldsymbol{\beta}_{R}}{\boldsymbol{\beta}_{1}, \ldots, \boldsymbol{\beta}_{R}} parameters in the model.
  }
  \item{varianceCovarianceUSamples}{
    The matrix of simulated samples from the posterior 
    distribution of \eqn{\boldsymbol{\Sigma}_{\boldsymbol{u}}}{\boldsymbol{\Sigma}_{\boldsymbol{u}}} in the model.
  }
  \item{uRandomEffectsSamples}{
    The matrix of simulated samples from the posterior 
    distribution of network random effects \eqn{\boldsymbol{u}_{1}, \ldots, \boldsymbol{u}_{J}, \boldsymbol{u}^{*}}{\boldsymbol{u}_{1}, \ldots, \boldsymbol{u}_{J}, \boldsymbol{u}^{*}} in the model.
  }
  \item{sigmaSquaredESamples}{
    The vector of simulated samples from the posterior 
    distribution of \eqn{\sigma_{e1}^{2}}{\sigma_{e1}^{2}}, \eqn{\ldots}{\ldots},  \eqn{\sigma_{eR}^{2}}{\sigma_{eR}^{2}} in the model. Only used if \eqn{\texttt{family}}{\texttt{family}}=``gaussian".
  }
  \item{acceptanceRates}{
    The acceptance rates of parameters in the model from the MCMC 
    sampling scheme .
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


