## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "85%",
  fig.align = "center",
  error = TRUE
)

# check if MSwM is installed and if not install
if(!require("MSwM")){
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages("MSwM")
}
if(!require("scales")){
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages("scales")
} 

## ----LaMa---------------------------------------------------------------------
library(LaMa)

head(trex)

## ----TapeConfig, include=FALSE------------------------------------------------
old <- RTMB::TapeConfig()
RTMB::TapeConfig(matmul = "plain") # speeds up forward algorithm

## ----tod2---------------------------------------------------------------------
modmat = make_matrices(~ s(tod, bs = "cp"), # formula
                       data = data.frame(tod = 1:24), # data
                       knots = list(tod = c(0, 24))) # where to wrap the cyclic basis
Z = modmat$Z # spline design matrix
S = modmat$S # penalty matrix

## ----mllk3--------------------------------------------------------------------
pnll = function(par) {
  getAll(par, dat)
  # cbinding intercept and spline coefs, because intercept is not penalised
  Gamma = tpm_g(Z, cbind(beta0, betaSpline))
  # computing all periodically stationary distributions for easy access later
  Delta = stationary_p(Gamma); REPORT(Delta)
  # parameter transformations
  mu = exp(logmu); REPORT(mu)
  sigma = exp(logsigma); REPORT(sigma)
  kappa = exp(logkappa); REPORT(kappa)
  # calculating all state-dependent densities
  allprobs = matrix(1, nrow = length(step), ncol = N)
  ind = which(!is.na(step) & !is.na(angle)) # only for non-NA obs.
  for(j in 1:N){
    allprobs[ind,j] = dgamma2(step[ind],mu[j],sigma[j]) * dvm(angle[ind],0,kappa[j])
  }
  -forward_g(Delta[tod[1],], Gamma[,,tod], allprobs) + # regular forward algorithm
    penalty(betaSpline, S, lambda) # this does all the penalisation work
}

## ----todpar2------------------------------------------------------------------
par = list(logmu = log(c(0.3, 2.5)), # state-dependent mean step
           logsigma = log(c(0.3, 2)), # state-dependent sd step
           logkappa = log(c(0.1, 2)), # state-dependent concentration angle
           beta0 = c(-2, -2), # state process intercepts
           betaSpline = matrix(rep(0, 2*(ncol(Z)-1)), nrow = 2)) # spline coefs

dat = list(step = trex$step, # observed steps
           angle = trex$angle, # observed angle
           N = 2, # number of states
           tod = trex$tod, # time of day (used for indexing)
           Z = Z, # spline design matrix
           S = S, # penalty matrix
           lambda = rep(100, 2)) # initial penalty strength

## ----trex_fit, message = FALSE------------------------------------------------
system.time(
  mod1 <- qreml(pnll, par, dat, random = "betaSpline")
)

## ----results qreml, fig.width = 9, fig.height = 5-----------------------------
Delta = mod1$Delta

tod_seq = seq(0, 24, length = 100)
Z_p = predict(modmat, data.frame(tod = tod_seq))

Gamma_plot = tpm_g(Z_p, mod1$beta) # interpolating transition probs

plot(tod_seq, Gamma_plot[1,2,], type = "l", lwd = 2, ylim = c(0,1),
     xlab = "time of day", ylab = "transition probability", bty = "n")
lines(tod_seq, Gamma_plot[2,1,], lwd = 2, lty = 3)
legend("topleft", lwd = 2, lty = c(1,3), bty = "n",
       legend = c(expression(gamma[12]^(t)), expression(gamma[21]^(t))))
plot(Delta[,2], type = "b", lwd = 2, pch = 16, xlab = "time of day", ylab = "Pr(active)", 
     col = "deepskyblue", bty = "n", xaxt = "n")
axis(1, at = seq(0,24,by=4), labels = seq(0,24,by=4))

## ----shark_data, fig.width = 9, fig.height = 5--------------------------------
head(nessi)
hist(nessi$logODBA, prob = TRUE, breaks = 50, bor = "white", 
     main = "", xlab = "log(ODBA)")

## ----nll Gaussian-------------------------------------------------------------
nll = function(par){
    getAll(par, dat)
    sigma = exp(logsigma) # exp because strictly positive
    REPORT(mu); REPORT(sigma)
    Gamma = tpm(eta) # multinomial logit link
    delta = stationary(Gamma) # stationary dist of the homogeneous Markov chain
    allprobs = matrix(1, length(logODBA), N)
    ind = which(!is.na(logODBA))
    for(j in 1:N) allprobs[ind,j] = dnorm(logODBA[ind], mu[j], sigma[j])
    -forward(delta, Gamma, allprobs)
}

## ----Gaussian fit, fig.width = 9, fig.height = 5------------------------------
# initial parameter list
par = list(mu = c(-4.5, -3.5, -2.5),
           logsigma = log(rep(0.5, 3)),
           eta = rep(-2, 6))

# data and hyperparameters
dat = list(logODBA = nessi$logODBA, N = 3)

# creating automatically differentiable objective function
obj = MakeADFun(nll, par, silent = TRUE)

# fitting the model
opt = nlminb(obj$par, obj$fn, obj$gr)

# reporting to get calculated quantities
mod = obj$report()

# visualising the results
color = c("orange", "deepskyblue", "seagreen3")
hist(nessi$logODBA, prob = TRUE, breaks = 50, bor = "white",
     main = "", xlab = "log(ODBA)")
for(j in 1:3) curve(mod$delta[j] * dnorm(x, mod$mu[j], mod$sigma[j]), 
                    add = TRUE, col = color[j], lwd = 2, n = 500)


## ----smooth prep--------------------------------------------------------------
# providing initial means and sds to initialise spline coefficients
par0 = list(logODBA = list(mean = c(-4, -3.3, -2.8), sd = c(0.3, 0.2, 0.5)))

# construct the smooth density objects
modmat = smooth_dens_construct(nessi["logODBA"], # only one data stream here
                               par = par0)

# par is nested named list: top layer: each data stream
# for each data stream: initial means and standard deviations for each state

# objects for model fitting
Z = modmat$Z$logODBA # spline design matrix for logODBA
S = modmat$S$logODBA # penalty matrix for logODBA
beta = modmat$coef$logODBA # initial spline coefficients

# objects for prediction
Z_p = modmat$Z_predict$logODBA # prediction design matrix
xseq = modmat$xseq$logODBA # prediction sequence of logODBA values

## ----pnll2--------------------------------------------------------------------
pnll = function(par){
  getAll(par, dat)
  # regular stationary HMM stuff
  Gamma = tpm(eta)
  delta = stationary(Gamma)
  # smooth state-dependent densities
  alpha = exp(cbind(beta, 0))
  alpha = alpha / rowSums(alpha) # multinomial logit link
  REPORT(alpha)
  allprobs = matrix(1, nrow(Z), N)
  ind = which(!is.na(Z[,1])) # only for non-NA obs.
  allprobs[ind,] = Z[ind,] %*% t(alpha)
  # forward algorithm + penalty
  -forward(delta, Gamma, allprobs) + 
    penalty(beta, S, lambda)
}

## ----nessi_spline_fit---------------------------------------------------------
par = list(beta = beta, # spline coefficients prepared by smooth_dens_construct()
           eta = rep(-2, 6)) # initial transition matrix on logit scale

dat = list(N = 3, # number of states
           Z = Z, # spline design matrix
           S = S, # spline penalty matrix
           lambda = rep(10, 3)) # initial penalty strength vector

# fitting the model using qREML
system.time(
  mod2 <- qreml(pnll, par, dat, random = "beta")
)

## ----shark_smooth_results, fig.width = 9, fig.height = 5----------------------
sDens = Z_p %*% t(mod2$alpha) # all three state-dependent densities on a grid

hist(nessi$logODBA, prob = TRUE, breaks = 50, bor = "white", main = "", xlab = "log(ODBA)")
for(j in 1:3) lines(xseq, mod2$delta[j] * sDens[,j], col = color[j], lwd = 2)
lines(xseq, colSums(mod2$delta * t(sDens)), col = "black", lwd = 2, lty = 2)

## ----energy_data--------------------------------------------------------------
data(energy, package = "MSwM")
head(energy)

## ----modmat_energy------------------------------------------------------------
modmat = make_matrices(~ s(Oil, k = 12, bs = "ps"), energy)
Z = modmat$Z # design matrix
S = modmat$S # penalty matrix (list)

## ----pnll3--------------------------------------------------------------------
pnll = function(par) {
  getAll(par, dat)
  Gamma = tpm(eta) # computing the tpm
  delta = stationary(Gamma) # stationary distribution

  # regression parameters for mean and sd
  beta = cbind(beta0, betaSpline); REPORT(beta) # mean parameter matrix
  alpha = cbind(alpha0, alphaSpline); REPORT(alpha) # sd parameter matrix

  # calculating all covariate-dependent means and sds
  Mu = Z %*% t(beta) # mean
  Sigma = exp(Z %*% t(alpha)) # sd

  allprobs = cbind(dnorm(price, Mu[,1], Sigma[,1]),
                   dnorm(price, Mu[,2], Sigma[,2])) # state-dependent densities
  
  - forward(delta, Gamma, allprobs) +
    penalty(list(betaSpline, alphaSpline), S, lambda)
}

## ----energy_fit---------------------------------------------------------------
# initial parameter list
par = list(eta = rep(-4, 2), # state process intercepts
           beta0 = c(2, 5), # state-dependent mean intercepts
           betaSpline = matrix(0, nrow = 2, ncol = 11), # mean spline coef
           alpha0 = c(0, 0), # state-dependent sd intercepts
           alphaSpline = matrix(0, nrow = 2, ncol = 11)) # sd spline coef

# data, model matrices and initial penalty strength
dat = list(price = energy$Price, 
           Z = Z, 
           S = S, 
           lambda = rep(1e3, 4))

# model fit
system.time(
  mod3 <- qreml(pnll, par, dat, random = c("betaSpline", "alphaSpline"))
)

## ----energy_results, fig.width = 9, fig.height = 5----------------------------
xseq = seq(min(energy$Oil), max(energy$Oil), length = 200) # sequence for prediction
Z_p = predict(modmat, newdata = data.frame(Oil = xseq)) # prediction design matrix

energy$states = viterbi(mod = mod3) # decoding most probable state sequence

Mu_plot = Z_p %*% t(mod3$beta)
Sigma_plot = exp(Z_p %*% t(mod3$alpha))

library(scales) # to make colors semi-transparent

par(mfrow = c(1,2))

# state-dependent distribution as a function of oil price
plot(energy$Oil, energy$Price, pch = 20, bty = "n", col = alpha(color[energy$states], 0.1),
     xlab = "oil price", ylab = "energy price")
for(j in 1:2) lines(xseq, Mu_plot[,j], col = color[j], lwd = 3) # means
qseq = qnorm(seq(0.5, 0.95, length = 4)) # sequence of quantiles
for(i in qseq){ for(j in 1:2){
  lines(xseq, Mu_plot[,j] + i * Sigma_plot[,j], col = alpha(color[j], 0.7), lty = 2)
  lines(xseq, Mu_plot[,j] - i * Sigma_plot[,j], col = alpha(color[j], 0.7), lty = 2)
}}
legend("topright", bty = "n", legend = paste("state", 1:2), col = color, lwd = 3)

# decoded time series
plot(NA, xlim = c(0, nrow(energy)), ylim = c(1,10), bty = "n",
     xlab = "time", ylab = "energy price")
segments(x0 = 1:(nrow(energy)-1), x1 = 2:nrow(energy),
         y0 = energy$Price[-nrow(energy)], y1 = energy$Price[-1], 
         col = color[energy$states[-1]], lwd = 0.5)


## ----restore_tapeConfig, include = FALSE--------------------------------------
RTMB::TapeConfig(old) # restoring old config

