## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  # fig.path = "img/",
  fig.align = "center",
  fig.dim = c(8, 6),
  out.width = "85%"
)

## ----setup--------------------------------------------------------------------
# loading the package
library(LaMa)

## ----parameters---------------------------------------------------------------
# generator matrix Q:
Q = matrix(c(-0.5, 0.5, 1, -1), 
           nrow = 2, byrow = TRUE)

# parameters for the state-dependent (normal) distributions
mu = c(5, 20)
sigma = c(2, 5)

## ----data---------------------------------------------------------------------
set.seed(123)

k = 200 # number of state switches
trans_times = s = rep(NA, k) # time points where the chain transitions
s[1] = sample(1:2, 1) # initial distribution c(0.5, 0.5)
# exponentially distributed waiting times
trans_times[1] = rexp(1, -Q[s[1],s[1]])
n_arrivals = rpois(1, trans_times[1])
obs_times = sort(runif(n_arrivals, 0, trans_times[1]))
x = rnorm(n_arrivals, mu[s[1]], sigma[s[1]])
for(t in 2:k){
  s[t] = c(1,2)[-s[t-1]] # for 2-states, always a state switch when transitioning
  # exponentially distributed waiting times
  trans_times[t] = trans_times[t-1] + rexp(1, -Q[s[t], s[t]])
  n_arrivals = rpois(1, trans_times[t]-trans_times[t-1])
  obs_times = c(obs_times, 
                sort(runif(n_arrivals, trans_times[t-1], trans_times[t])))
  x = c(x, rnorm(n_arrivals, mu[s[t]], sigma[s[t]]))
}

## ----vis_ctHMM----------------------------------------------------------------
color = c("orange", "deepskyblue")

n = length(obs_times)
plot(obs_times[1:50], x[1:50], pch = 16, bty = "n", xlab = "observation times", 
     ylab = "x", ylim = c(-5,25))
segments(x0 = c(0,trans_times[1:48]), x1 = trans_times[1:49], 
         y0 = rep(-5,50), y1 = rep(-5,50), col = color[s[1:49]], lwd = 4)
legend("topright", lwd = 2, col = color, 
       legend = c("state 1", "state 2"), box.lwd = 0)

## ----mllk---------------------------------------------------------------------
nll = function(par, timediff, x, N){
  mu = par[1:N]
  sigma = exp(par[N+1:N])
  Q = generator(par[2*N+1:(N*(N-1))]) # generator matrix
  Pi = stationary_cont(Q) # stationary dist of CT Markov chain
  Qube = tpm_cont(Q, timediff) # this computes exp(Q*timediff)
  allprobs = matrix(1, nrow = length(x), ncol = N)
  ind = which(!is.na(x))
  for(j in 1:N){
    allprobs[ind,j] = dnorm(x[ind], mu[j], sigma[j])
  }
  -forward_g(Pi, Qube, allprobs)
}

## ----model, warning=FALSE-----------------------------------------------------
par = c(mu = c(5, 15), # state-dependent means
        logsigma = c(log(3), log(5)), # state-dependent sds
        qs = c(log(1), log(0.5))) # off-diagonals of Q

timediff = diff(obs_times)

system.time(
  mod <- nlm(nll, par, timediff = timediff, x = x, N = 2)
)

## ----results------------------------------------------------------------------
N = 2
# mu
round(mod$estimate[1:N],2)
# sigma
round(exp(mod$estimate[N+1:N]))
Q = generator(mod$estimate[2*N+1:(N*(N-1))])
round(Q,3)

## ----parameters2--------------------------------------------------------------
# generator matrix Q:
Q = matrix(c(-0.5,0.2,0.3,
             1,-2, 1,
             0.4, 0.6, -1), nrow = 3, byrow = TRUE)

# parameters for the state-dependent (normal) distributions
mu = c(5, 15, 30)
sigma = c(2, 3, 5)

## ----data2--------------------------------------------------------------------
set.seed(123)

k = 200 # number of state switches
trans_times = s = rep(NA, k) # time points where the chain transitions
s[1] = sample(1:3, 1) # uniform initial distribution
# exponentially distributed waiting times
trans_times[1] = rexp(1, -Q[s[1],s[1]])
n_arrivals = rpois(1, trans_times[1])
obs_times = sort(runif(n_arrivals, 0, trans_times[1]))
x = rnorm(n_arrivals, mu[s[1]], sigma[s[1]])
for(t in 2:k){
  # off-diagonal elements of the s[t-1] row of Q divided by the diagonal element
  # give the probabilities of the next state
  s[t] = sample(c(1:3)[-s[t-1]], 1, prob = Q[s[t-1],-s[t-1]]/-Q[s[t-1],s[t-1]])
  # exponentially distributed waiting times
  trans_times[t] = trans_times[t-1] + rexp(1, -Q[s[t], s[t]])
  n_arrivals = rpois(1, trans_times[t]-trans_times[t-1])
  obs_times = c(obs_times, 
                sort(runif(n_arrivals, trans_times[t-1], trans_times[t])))
  x = c(x, rnorm(n_arrivals, mu[s[t]], sigma[s[t]]))
}

## ----model2, warning=FALSE----------------------------------------------------
par = c(mu = c(5, 10, 25), # state-dependent means
        logsigma = c(log(2), log(2), log(6)), # state-dependent sds
        qs = rep(0, 6)) # off-diagonals of Q

timediff = diff(obs_times)

system.time(
  mod2 <- nlm(nll, par, timediff = timediff, x = x, N = 3, stepmax = 10)
)
# without restricting stepmax, we run into numerical problems

## ----results2-----------------------------------------------------------------
N = 3
# mu
round(mod2$estimate[1:N],2)
# sigma
round(exp(mod2$estimate[N+1:N]),2)
Q = generator(mod2$estimate[2*N+1:(N*(N-1))])
round(Q, 3)

