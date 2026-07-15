## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  # fig.path = "img/",
  fig.align = "center",
  fig.dim = c(8, 6),
  out.width = "85%"
)

# check if MSwM is installed and if not install
if(!require("PHSMM")){
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages("PHSMM")
}

## ----setup--------------------------------------------------------------------
# loading the package
library(LaMa)

## ----parameters---------------------------------------------------------------
lambda = c(7, 4, 4)
omega = matrix(c(0, 0.7, 0.3,
                 0.5, 0, 0.5,
                 0.7, 0.3, 0), nrow = 3, byrow = TRUE)
mu = c(10, 40, 100)
sigma = c(5, 20, 50)

color = c("orange", "deepskyblue", "seagreen2")
curve(dnorm(x, mu[1], sigma[1]), lwd = 2, col = color[1], bty = "n",
      xlab = "x", ylab = "density", xlim = c(0, 150), n = 300)
curve(dnorm(x, mu[2], sigma[2]), lwd = 2, col = color[2], add = T)
curve(dnorm(x, mu[3], sigma[3]), lwd = 2, col = color[3], add = T)

## ----simulation---------------------------------------------------------------
set.seed(123)

k = 50 # number of stays
s = rep(NA, k)
s[1] = sample(1:3, 1) # uniform initial distribution
staylength = rpois(1, lambda[s[1]]) + 1 # drawing dwell time from shifted Poisson
C = rep(s[1], staylength)
x = rnorm(staylength, mu[s[1]], sigma[s[1]])

for(t in 2:k){
  # conditionally drawing state
  s[t] = sample(c(1:3)[-s[t-1]], 1, prob = omega[s[t-1], -s[t-1]])
  staylength = rpois(1, lambda[s[t]]) + 1 # drawing dwell time from shifted Poisson
  
  C = c(C, rep(s[t], staylength))
  x = c(x, rnorm(staylength, mu[s[t]], sigma[s[t]]))
}

plot(x, pch = 20, col = color[C], bty = "n")
legend("topright", col = color, pch = 20, 
       legend = paste("state", 1:3), box.lwd = 0)

## ----mllk---------------------------------------------------------------------
nll = function(par, x, N, agsizes){
  mu = par[1:N]
  sigma = exp(par[N+1:N])
  lambda = exp(par[2*N+1:N])
  omega = if(N==2) tpm_emb() else tpm_emb(par[3*N+1:(N*(N-2))])
  dm = list() # list of dwell-time distributions
  for(j in 1:N) dm[[j]] = dpois(1:agsizes[j]-1, lambda[j]) # shifted Poisson
  Gamma = tpm_hsmm(omega, dm, sparse = FALSE)
  delta = stationary(Gamma)
  allprobs = matrix(1, length(x), N)
  ind = which(!is.na(x))
  for(j in 1:N){
    allprobs[ind,j] = dnorm(x[ind], mu[j], sigma[j])
  }
  -forward_s(delta, Gamma, allprobs, agsizes)
}

## ----model--------------------------------------------------------------------
# initial values
par = c(10, 40, 100, log(c(5, 20, 50)), # state-dependent
               log(c(7,4,4)), # dwell time means
               rep(0, 3)) # omega

agsizes = qpois(0.95, lambda)+1

system.time(
  mod <- nlm(nll, par, x = x, N = 3, agsizes = agsizes, stepmax = 2)
)

## ----results------------------------------------------------------------------
N = 3
(mu = mod$estimate[1:N])
(sigma = exp(mod$estimate[N+1:N]))
(lambda = exp(mod$estimate[2*N+1:N]))
(omega = tpm_emb(mod$estimate[3*N+1:(N*(N-2))]))

## ----muskox_data--------------------------------------------------------------
library(PHSMM)
data = muskox[1:1000, ] # only using first 1000 observations for speed
head(data)

## ----muskox_likelihood--------------------------------------------------------
nll_muskox = function(par, step, N, agsizes){
  # parameter transformation from working to natural
  mu = exp(par[1:N]) # step mean
  sigma = exp(par[N+1:N]) # step standard deviation
  mu_dwell = exp(par[2*N+1:N]) # dwell time mean
  phi = exp(par[3*N+1:N]) # dwell time dispersion
  omega = if(N==2) tpm_emb() else tpm_emb(par[4*N+1:(N*(N-2))])
  dm = list() # list of dwell-time distributions
  for(j in 1:N){ 
    dm[[j]] = dnbinom(1:agsizes[j]-1, mu=mu_dwell[j], size=1/phi[j]) 
  }
  Gamma = tpm_hsmm(omega, dm, sparse = FALSE)
  delta = stationary(Gamma)
  allprobs = matrix(1, length(step), N)
  ind = which(!is.na(step))
  for(j in 1:N) allprobs[ind,j] = dgamma2(step[ind], mu[j], sigma[j])
  -forward_s(delta, Gamma, allprobs, agsizes)
}

## ----model_muskox, warning = FALSE, cache = TRUE------------------------------
# intial values
par = c(log(c(4, 50, 300, 4, 50, 300)), # state-dependent mean and sd
               log(c(3,3,5)), # dwell time means
               log(c(0.01, 0.01, 0.01)), # dwell time dispersion
               rep(0, 3)) # omega

agsizes = c(11,11,14)

system.time(
  mod_muskox <- nlm(nll_muskox, par, step = data$step, N = 3,
                    agsizes = agsizes, iterlim = 500)
)

## ----results_muskox-----------------------------------------------------------
par = mod_muskox$estimate; N = 3
(mu = exp(par[1:N])) # step mean
(sigma = exp(par[N+1:N])) # step standard deviation
(mu_dwell = exp(par[2*N+1:N])) # dwell time mean
(phi = exp(par[3*N+1:N])) # dwell time dispersion
(omega = tpm_emb(par[4*N+1:(N*(N-2))])) # embedded t.p.m.

## ----dwell_muskox-------------------------------------------------------------
oldpar = par(mfrow = c(1,3))
for(j in 1:N){
  plot(1:agsizes[j], dnbinom(1:agsizes[j]-1, mu=mu_dwell[j], size = 1/phi[j]),
       type = "h", lwd = 2, col = color[j], xlab = "dwell time (hours)",
       ylab = "probabilities", main = paste("state",j), bty = "n", ylim = c(0,0.25))
}
par(oldpar)

