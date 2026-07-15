## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  # fig.path = "img/",
  fig.align = "center",
  fig.dim = c(8, 6),
  out.width = "85%"
)

## ----setup and data-----------------------------------------------------------
library(LaMa)

head(trex, 5)

## ----nll multiple data streams------------------------------------------------
nll_multi = function(par) {
  getAll(par, dat)
  Gamma = tpm(eta)
  delta = stationary(Gamma)
  # Parameter transformations for strictly positive parameters
  mu = exp(log_mu); REPORT(mu)
  sigma = exp(log_sigma); REPORT(sigma)
  # additional state-dependent turning angle concentration parameter
  kappa = exp(log_kappa); REPORT(kappa)
  # Calculating all state-dependent densities
  allprobs = matrix(1, nrow = length(step), ncol = N)
  ind = which(!is.na(step) & !is.na(angle)) # only for non-NA obs.
  for(j in 1:N){
    allprobs[ind,j] = dgamma2(step[ind], mu[j], sigma[j]) * 
      dvm(angle[ind], 0, kappa[j]) # simply multiplying the state-dep. densities
  }
  -forward(delta, Gamma, allprobs) # simple forward algorithm
}

## ----multiple streams fit-----------------------------------------------------
N = 2 # number of hidden states

par = list(
  eta = rep(-2, N*(N-1)),                   # (logit) initial t.p.m. parameters
  log_mu = log(seq(0.3, 1, length=N)),      # (log) initial means for step length
  log_sigma = log(seq(0.2, 0.7, length=N)), # (log) initial sds for step length 
  log_kappa = log(seq(0.2, 0.4, length=N))  # (log) initial concentration for angle
)    

dat = list(
  step = trex$step,   # hourly step lengths
  angle = trex$angle, # hourly turning angles
  N = N
)

# Fitting the model
obj_multi = MakeADFun(nll_multi, par, silent = TRUE)
opt_multi = nlminb(obj_multi$par, obj_multi$fn, obj_multi$gr)
mod_multi = report(obj_multi) # reporting from fitted model

## ----state dist multi, fig.width = 8, fig.height = 4--------------------------
# extracting estimated parameters
delta = mod_multi$delta
mu = mod_multi$mu
sigma = mod_multi$sigma
kappa = mod_multi$kappa

# defining color vector
color = c("orange", "deepskyblue")

oldpar = par(mfrow = c(1,2))

# step length
hist(trex$step, prob = TRUE, breaks = 40, 
     bor = "white", main = "", xlab = "step length")
for(j in 1:N) curve(delta[j] * dgamma2(x, mu[j], sigma[j]), lwd = 2, add = TRUE, col = color[j])
curve(delta[1]*dgamma2(x, mu[1], sigma[1]) + delta[2]*dgamma2(x, mu[2], sigma[2]), 
      lwd = 2, lty = 2, add = TRUE)
legend("top", lwd = 2, col = c(color, 1), lty = c(1,1,2), 
       legend = c("state 1", "state 2", "marginal"), bty = "n")

# turning angle
hist(trex$angle, prob = TRUE, breaks = seq(-pi, pi, length = 20), 
     bor = "white", main = "", xlab = "turning angle")
for(j in 1:2) curve(delta[j] * dvm(x, 0, kappa[j]), lwd = 2, add = TRUE, col = color[j])
curve(delta[1] * dvm(x, 0, kappa[1]) + delta[2] * dvm(x, 0, kappa[2]), 
      lwd = 2, lty = 2, add = TRUE)
par(oldpar)

## ----parameters---------------------------------------------------------------
# parameters
mu = c(5, 20)   # state-dependent means
sigma = c(4, 5) # state-dependent standard deviations

# state process regression parameters
beta = matrix(c(-2, -2,       # intercepts
                -1, 1.5,      # linear effects
                0.25, -0.5),  # quadratic effects
              nrow = 2)

n = 1000 # number of observations
set.seed(123)
z = rnorm(n)
Z = cbind(z, z^2) # quadratic effect of z
Gamma = tpm(beta, Z) # of dimension c(2, 2, n)
delta = c(0.5, 0.5) # non-stationary initial distribution


# Let's plot the covariate effects that we simulated:
z_p = seq(-2,2, length = 100) # prediction grid for covariate plot
Z_p = cbind(z_p, z_p^2) # prediction matrix
Gamma_p = tpm(beta, Z_p)
plot(z_p, Gamma_p[1,2,], type = "l", lwd = 3, bty = "n", ylim = c(0,1), 
     xlab = "z", ylab = expression(gamma[ij]), col = color[1])
lines(z_p, Gamma_p[2,1,], lwd = 3, col = color[2])

## ----data---------------------------------------------------------------------
s = rep(NA, n)
s[1] = sample(1:2, 1, prob = delta) # sampling first state from initial distribution
for(t in 2:n){
  # sampling next state conditional on previous one with tpm at that time point
  s[t] = sample(1:2, 1, prob = Gamma[s[t-1],,t])
}
# sampling observations conditional on the states
x = rnorm(n, mu[s], sigma[s])

plot(x[1:200], bty = "n", pch = 20, ylab = "x", 
     col = c(color[1], color[2])[s[1:200]])

## ----mllk---------------------------------------------------------------------
nll_cov = function(par) {
  getAll(par, dat)
  Gamma = tpm(beta, Z)
  # unconstrained parameterisation of a 2-state initial distribution
  delta = c(1, exp(logit_delta)) # make > 0
  delta = delta / sum(delta) # distribution -> needs to sum to 1
  sigma = exp(log_sigma); REPORT(sigma); REPORT(mu)
  # calculate all state-dependent probabilities
  allprobs = matrix(1, length(x), N)
  for(j in 1:N) allprobs[,j] = dnorm(x, mu[j], sigma[j])
  # forward algorithm
  -forward(delta, Gamma, allprobs)
}

## ----model, warning=FALSE-----------------------------------------------------
par = list(
  beta = cbind(rep(-2, 2), matrix(0, 2, 2)), # initialising with slopes = 0
  logit_delta = 0,                           # starting value for initial distribution
  mu = c(4, 14),                             # initial state-dependent means
  log_sigma = c(log(3), log(5))              # initial state-dependent sds
)
dat <- list(
  x = x, 
  Z = Z,
  N = 2
)

obj_cov = MakeADFun(nll_cov, par, silent = TRUE)
system.time(
  opt <- nlminb(obj_cov$par, obj_cov$fn, obj_cov$gr)
)
mod_cov <- report(obj_cov) # reporting from fitted model

## ----mod_cov_densities--------------------------------------------------------
mod_cov$states = viterbi(mod = mod_cov)
delta = prop.table(table(mod_cov$states))

# get state-dependent parameters
mu = mod_cov$mu
sigma = mod_cov$sigma

hist(x, prob = TRUE, bor = "white", breaks = 20, main = "", ylim = c(0, 0.06))
curve(delta[1] * dnorm(x, mu[1], sigma[1]), add = TRUE, lwd = 3, col = color[1])
curve(delta[2] * dnorm(x, mu[2], sigma[2]), add = TRUE, lwd = 3, col = color[2])
curve(delta[1] * dnorm(x, mu[1], sigma[1]) +
        delta[2] * dnorm(x, mu[2], sigma[2]),
      add = TRUE, lwd = 3, lty = "dashed")
legend("topright", col = c(color[1], color[2], "black"), lwd = 3, bty = "n",
       lty = c(1,1,2), legend = c("state 1", "state 2", "marginal"))

## ----cov_visualization--------------------------------------------------------
samples = MCreport(obj_cov)

# compute samples
g12 = sapply(samples$beta, function(b) tpm(b, Z_p)[1,2,]) # gamma_12 
g21 = sapply(samples$beta, function(b) tpm(b, Z_p)[2,1,]) # gamma_21
# compute quantiles
g12.q = apply(g12, 1, quantile, probs = c(0.025, 0.975))
g21.q = apply(g21, 1, quantile, probs = c(0.025, 0.975))

# at MLE
Gamma_p = tpm(mod_cov$beta, Z_p) # predicted tpm

# plot fitted state-process regression
plot(NA, bty = "n", ylim = c(0,1), xlim = c(-2,2), 
     xlab = "z", ylab = "Transition probability")

# confidence interval
polygon(c(z_p, rev(z_p)), c(g12.q[1,], rev(g12.q[2,])), col = adjustcolor(color[1], 0.3), border = NA)
# MLE
lines(z_p, Gamma_p[1,2,], lwd = 3, col = color[1])

# confidence interval
polygon(c(z_p, rev(z_p)), c(g21.q[1,], rev(g21.q[2,])), col = adjustcolor(color[2], 0.3), border = NA)
# MLE
lines(z_p, Gamma_p[2,1,], lwd = 3, col = color[2])

legend("top", col = color, legend = c(expression(gamma[12]), expression(gamma[21])), lwd = 3, bty = "n")

## ----parameters2--------------------------------------------------------------
sigma = c(1, 1) # state-dependent standard deviations (homoscedasticity)

# parameter matrix
# each row contains parameter vector for the corresponding state
beta = matrix(c(8, 10,             # intercepts
                -2, 1, 0.5, -0.5), # slopes
              nrow = 2)

n = 1000 # number of observations
set.seed(123)
z = rnorm(n)
Z = cbind(z, z^2) # quadratic effect of z

Gamma = matrix(c(0.9, 0.1, 0.05, 0.95), 
               nrow = 2, byrow = TRUE) # homogeneous t.p.m.
delta = stationary(Gamma) # stationary Markov chain

## ----data2--------------------------------------------------------------------
s = x = rep(NA, n)
s[1] = sample(1:2, 1, prob = delta)
x[1] = rnorm(1, beta[s[1],]%*%c(1, Z[1,]), # state-dependent regression
                    sigma[s[1]])
for(t in 2:n){
  s[t] = sample(1:2, 1, prob = Gamma[s[t-1],])
  x[t] = rnorm(1, beta[s[t],] %*% c(1, Z[t,]), # state-dependent regression
                      sigma[s[t]])
}

oldpar = par(mfrow = c(1,2))
plot(x[1:400], bty = "n", pch = 20, ylab = "x", 
     col = c(color[1], color[2])[s[1:400]])

plot(z[which(s==1)], x[which(s==1)], pch = 16, col = color[1], bty = "n", 
     ylim = c(0,15), xlab = "z", ylab = "x")
points(z[which(s==2)], x[which(s==2)], pch = 16, col = color[2])
par(oldpar)

## ----mllk3--------------------------------------------------------------------
nll_msr = function(par) {
  getAll(par, dat)
  Gamma = tpm(eta)
  delta = stationary(Gamma)
  sigma = exp(log_sigma); REPORT(sigma)
  # calculate all state-dependent probabilities
  allprobs = matrix(1, length(x), 2)
  # state-dependent regression
  REPORT(beta)
  Mu = cbind(1,Z) %*% t(beta) # Z %*% beta[j,] all at once
  for(j in 1:2) allprobs[,j] = dnorm(x, Mu[,j], sigma[j])
  # forward algorithm
  -forward(delta, Gamma, allprobs)
}

## ----model3, warning=FALSE----------------------------------------------------
par = list(
  eta = rep(-2, 2),                        # starting values state process
  beta = cbind(c(10, 10), matrix(0, 2, 2)), # starting values for regression
  log_sigma = c(log(1), log(1))            # starting values for sigma
)
dat = list(
  x = x,
  Z = Z
)

obj_msr = MakeADFun(nll_msr, par, silent = TRUE)
opt_msr = nlminb(obj_msr$par, obj_msr$fn, obj_msr$gr)

mod_msr = report(obj_msr)

## ----visualization3-----------------------------------------------------------
Gamma_hat_reg = mod_msr$Gamma
delta_hat_reg = mod_msr$delta
sigma_hat_reg = mod_msr$sigma
beta_hat_reg = mod_msr$beta

# we have some label switching
plot(z, x, pch = 16, bty = "n", xlab = "z", ylab = "x", col = color[s])
points(z, x, pch = 20)
curve(beta_hat_reg[1,1] + beta_hat_reg[1,2]*x + beta_hat_reg[1,3]*x^2, 
      add = TRUE, lwd = 4, col = color[2])
curve(beta_hat_reg[2,1] + beta_hat_reg[2,2]*x + beta_hat_reg[2,3]*x^2, 
      add = TRUE, lwd = 4, col = color[1])

## ----cosinor------------------------------------------------------------------
Z_tod = cosinor(1:24, period = 24)
head(Z_tod)

## ----trex data set------------------------------------------------------------
head(trex, 6)

## ----parameters seasonal------------------------------------------------------
par = list(
  log_mu = log(c(0.3, 1)),      # initial means for step length
  log_sigma = log(c(0.2, 0.7)), # initial sds for step length
  beta = cbind(c(-2,-2), matrix(0, 2, 2)) # initial t.p.m. parameters
)    

dat = list(
  step = trex$step,   # hourly step lengths
  N = 2,
  Z_tod = Z_tod,
  tod = trex$tod
)

## ----nll seasonal-------------------------------------------------------------
nll_seas = function(par) {
  getAll(par, dat)
  Gamma = tpm(beta, Z_tod)
  Delta = stationary_p(Gamma) # periodically stationary distribution
  ADREPORT(Delta)
  # Parameter transformations for strictly positive parameters
  mu = exp(log_mu); REPORT(mu)
  sigma = exp(log_sigma); REPORT(sigma)
  # Calculating all state-dependent densities
  allprobs = matrix(1, nrow = length(step), ncol = N)
  ind = which(!is.na(step)) # only for non-NA obs.
  for(j in 1:N){
    allprobs[ind,j] = dgamma2(step[ind], mu[j], sigma[j])
  }
  -forward(Delta[tod[1],], Gamma[,,tod], allprobs)
}

## ----seasonal fit-------------------------------------------------------------
obj_seas = MakeADFun(nll_seas, par, silent = TRUE)
opt_seas = nlminb(obj_seas$par, obj_seas$fn, obj_seas$gr)
sdr_seas = sdreport(obj_seas)
mod_seas = report(obj_seas)

## ----p stationary-------------------------------------------------------------
Delta = as.list(sdr_seas, "Est", report = TRUE)$Delta
Delta_sd = as.list(sdr_seas, "Std", report = TRUE)$Delta

# Only plot for state 2 (state 1 is just 1-delta_2)
id = c(24, 1:24) # copy value at 24 to 0 for nicer plot
plot(0:24, Delta[id,2], type = "b", col = "deepskyblue", bty = "n", lwd = 2, 
     pch = 16, ylim = c(0,1), xlim = c(0, 24), 
     xlab = "time of day", ylab = "Pr(state 2)", xaxt = "n")
axis(1, at = c(0, 6, 12, 18, 24), label = c(0, 6, 12, 18, 24))
polygon(c(0:24, 24:0), 
        c(Delta[id,2] + 2*Delta_sd[id,2], rev(Delta[id,2] - 2*Delta_sd[id,2])),
        col = adjustcolor("deepskyblue", 0.2), border = FALSE)

