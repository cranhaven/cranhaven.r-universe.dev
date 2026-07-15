## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  # fig.path = "img/",
  fig.align = "center",
  fig.dim = c(8, 6),
  out.width = "75%"
)

options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
# loading the package
library(LaMa)

## ----data_pooling-------------------------------------------------------------
# parameters are shared across individuals
mu = c(15, 60) # state-dependent means
sigma = c(10, 40) # state-dependent standard deviations
Gamma = matrix(c(0.95, 0.05, 0.15, 0.85), nrow = 2, byrow = TRUE) # t.p.m.
delta = stationary(Gamma) # stationary distribution

# simulation of all tracks
set.seed(123)
K = 200 # number of individuals, for example different animals
n = 50 # observations per animal only (but many animals)

s = x = rep(NA, n*K)
for(k in 1:K){
  sk = xk = rep(NA, n)
  sk[1] = sample(1:2, 1, prob = delta)
  xk[1] = rnorm(1, mu[sk[1]], sigma[sk[1]])
  for(t in 2:n){
    sk[t] = sample(1:2, 1, prob = Gamma[sk[t-1],]) 
    xk[t] = rnorm(1, mu[sk[t]], sigma[sk[t]])
  }
  s[(k-1)*n + 1:n] = sk
  x[(k-1)*n + 1:n] = xk
}

trackID = rep(1:K, each = n)

## ----mllk_pool----------------------------------------------------------------
# fast version using trackInd in forward()
nll_pool = function(par, x, trackID){
  Gamma = tpm(par[1:2])
  delta = stationary(Gamma)
  mu = par[3:4]
  sigma = exp(par[5:6])
  allprobs = matrix(1, length(x), 2)
  for(j in 1:2) allprobs[,j] = dnorm(x, mu[j], sigma[j])
  
  # here we add trackInd as an argument to forward()
  -forward(delta, Gamma, allprobs, trackID)
}

# slow alternative looping over individuals in R
nll_pool_slow = function(par, x, K){
  n = length(x) / K
  Gamma = tpm(par[1:2])
  delta = stationary(Gamma)
  mu = par[3:4]
  sigma = exp(par[5:6])
  allprobs = matrix(1, length(x), 2)
  for(j in 1:2) allprobs[,j] = dnorm(x, mu[j], sigma[j])
  
  # here we just loop over individuals in R
  l = 0
  for(k in 1:K){
    l = l + forward(delta, Gamma, allprobs[(k-1)*n + 1:n,])
  }
  -l
}

## ----model_pool, warning=FALSE, cache = TRUE----------------------------------
# initial parameter vector
par = c(logitgamma = c(-1,-1), # off-diagonals of Gamma (on logit scale)
        mu = c(15, 60), # state-dependent means
        logsigma = c(log(10),log(40))) # state-dependent sds

# fast version:
system.time(
  mod <- nlm(nll_pool, par, x = x, trackID = trackID)
)

# slow version
system.time(
  mod <- nlm(nll_pool_slow, par, x = x, K = K)
)

## ----data_partial-------------------------------------------------------------
K = 5 # number of individuals, for example different animals

# state-dependent parameters are shared across individuals
mu = c(15, 60)
sigma = c(10, 40)

# but we define a tpm for each individual depending on covariates
set.seed(123)
z = rnorm(K) # covariate (e.g. age)
beta = matrix(c(-2,-2, 1, -1), nrow = 2)
# we calculate 5 tpms depending on individual-specific covariates:
Gamma = tpm_g(z, beta)
# each individual starts in its stationary distribution:
Delta = matrix(NA, K, 2)
for(k in 1:K){ Delta[k,] = stationary(Gamma[,,k]) }

# simulation of all tracks
set.seed(123)
n = 200 # observations per animal only (but many animals)
s = x = rep(NA, n*K)
for(k in 1:K){
  sk = xk = rep(NA, n)
  sk[1] = sample(1:2, 1, prob = Delta[k, ])
  xk[1] = rnorm(1, mu[sk[1]], sigma[sk[1]])
  for(t in 2:n){
    sk[t] = sample(1:2, 1, prob = Gamma[sk[t-1],,k]) 
    xk[t] = rnorm(1, mu[sk[t]], sigma[sk[t]])
  }
  s[(k-1)*n + 1:n] = sk
  x[(k-1)*n + 1:n] = xk
}

## ----mllk_partial-------------------------------------------------------------
# fast version using trackInd in forward()
nll_partial = function(par, x, z, trackID){
  # individual-specific tpms
  beta = matrix(par[1:4], nrow = 2)
  Gamma = tpm_g(z, beta)
  Delta = t(sapply(1:k, function(k) stationary(Gamma[,,k])))
  mu = par[5:6]
  sigma = exp(par[7:8])
  allprobs = matrix(1, length(x), 2)
  for(j in 1:2) allprobs[,j] = dnorm(x, mu[j], sigma[j])
  # just handing a Delta matrix and Gamma array for all individuals to forward()
  -forward(Delta, Gamma, allprobs, trackID)
}

## ----model_partial, warning=FALSE---------------------------------------------
# again defining all the indices where a new track begins
trackID = rep(1:K, each = n)

# initial parameter vector
par = c(beta = c(-2, -2, 0, 0), # beta
        mu = c(15, 60), # state-dependent means
        log(10), log(40)) # state-dependent sds

system.time(
  mod_partial <- nlm(nll_partial, par, x = x, z = z, trackID = trackID)
)

