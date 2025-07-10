### Purpose
Performs reversible-jump MCMC, a Bayesian multimodel inference method. The process is simpler than a manual implementation; for instance, all Jacobian matrices are automatically calculated using the madness package. The effort required to find Bayes factors and posterior model probabilities is reduced.

### Usage
For each model considered, the user requires a posterior distribution obtained via MCMC or the like. They then define a bijection between its parameter space and the universal parameter space; the likelihood model on the data; and the priors on the parameters. The `rjmcmcpost` function uses a post-processing algorithm to estimate posterior model probabilities. See `?rjmcmcpost` for a simple example using binomial likelihoods.

### Installation
`install.packages("rjmcmc")`
`library(rjmcmc)`