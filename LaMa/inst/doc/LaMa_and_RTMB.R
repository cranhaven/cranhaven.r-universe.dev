## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "85%",
  fig.align = "center",
  error = TRUE
)

## ----setup--------------------------------------------------------------------
library(LaMa)

## ----data---------------------------------------------------------------------
head(trex, 5)

## ----parameters---------------------------------------------------------------
par = list(
  log_mu = log(c(0.3, 1)),      # initial means for step length (log-transformed)
  log_sigma = log(c(0.2, 0.7)), # initial sds for step length (log-transformed)
  eta = rep(-2, 2)              # initial t.p.m. parameters (on logit scale)
  )    

dat = list(
  step = trex$step,   # hourly step lengths
  N = 2
  )

## ----mllk---------------------------------------------------------------------
nll = function(par) {
  getAll(par, dat) # makes everything contained available without $
  Gamma = tpm(eta) # computes transition probability matrix from unconstrained eta
  delta = stationary(Gamma) # computes stationary distribution
  # Parameter transformations for strictly positive parameters
  mu = exp(log_mu)
  sigma = exp(log_sigma)
  # Reporting statements for later use
  REPORT(mu); ADREPORT(mu)
  REPORT(sigma); ADREPORT(sigma)
  # Calculating all state-dependent densities
  allprobs = matrix(1, nrow = length(step), ncol = N)
  ind = which(!is.na(step)) # only for non-NA obs.
  for(j in 1:N){
    allprobs[ind,j] = dgamma2(step[ind], mu[j], sigma[j])
  }
  -forward(delta, Gamma, allprobs) # simple forward algorithm
}

## ----ADfunction, message = FALSE----------------------------------------------
obj = MakeADFun(nll, par, silent = TRUE) # creating the AD objective function

## ----tmbobject----------------------------------------------------------------
names(obj)

## ----tmbobject2---------------------------------------------------------------
obj$par
obj$fn()
obj$gr()

## ----modelfit-----------------------------------------------------------------
opt = nlminb(obj$par, obj$fn, obj$gr) # minimisation

## ----optpar-------------------------------------------------------------------
opt$par
opt$objective

## ----MLe----------------------------------------------------------------------
mod = report(obj) # runs the reporting from the negative log-likelihood once
(delta = mod$delta) # stationary distribution
(Gamma = mod$Gamma) # estimated tpm
(mu = mod$mu) # estimated state-dependent means
(sigma = mod$sigma) # estimated state-dependent sds

## ----decoding, fig.width = 7, fig.height = 4----------------------------------
# manually
mod$states = viterbi(mod$delta, mod$Gamma, mod$allprobs)

# or much simpler
mod$states = viterbi(mod = mod)

# defining color vector
color = c("orange", "deepskyblue")

plot(trex$step[1:200], type = "h", xlab = "time", ylab = "step length", 
     col = color[mod$states[1:200]], bty = "n")
legend("topright", col = color, lwd = 1, legend = c("state 1", "state 2"), bty = "n")

## ----statedepdist, fig.width = 8, fig.height = 4------------------------------
hist(trex$step, prob = TRUE, breaks = 40, 
     bor = "white", main = "", xlab = "step length")
for(j in 1:2) curve(delta[j] * dgamma2(x, mu[j], sigma[j]), 
                    lwd = 2, add = T, col = color[j])
curve(delta[1]*dgamma2(x, mu[1], sigma[1]) + delta[2]*dgamma2(x, mu[2], sigma[2]), 
      lwd = 2, lty = 2, add = T)
legend("top", lwd = 2, col = color, legend = c("state 1", "state 2"), bty = "n")

## ----AICBIC-------------------------------------------------------------------
AIC(mod)
BIC(mod)

## ----sdreport-----------------------------------------------------------------
sdr = sdreport(obj)

## ----sdreport2----------------------------------------------------------------
summary(sdr) # see ?summary.sdreport for more info

## ----sdreport3, eval = F------------------------------------------------------
# # estimated parameter in list format
# as.list(sdr, "Estimate")
# # standard errors in list format
# as.list(sdr, "Std")

## ----sdreport4, eval = F------------------------------------------------------
# # adreported parameters as list
# as.list(sdr, "Estimate", report = TRUE)
# # their standard errors
# as.list(sdr, "Std", report = TRUE)

## ----pres, fig.width = 8, fig.height = 4--------------------------------------
pres_step = pseudo_res(obs = trex$step, # observation sequence
                       dist = "gamma2", # which parametric CDF to use
                       par = list(mean = mu, sd = sigma), # estimated pars for that CDF
                       mod = mod) # model object
plot(pres_step, hist = TRUE)

## ----error, eval = FALSE------------------------------------------------------
# stop("Invalid argument to 'advector' (lost class attribute?)")

## ----overloading--------------------------------------------------------------
"[<-" <- ADoverload("[<-")

## ----overloading2-------------------------------------------------------------
"c" <- ADoverload("c")
"diag<-" <- ADoverload("diag<-")

## ----NA, eval = FALSE---------------------------------------------------------
# X = array(dim = c(1,2,3))
# # which is the same as
# X = array(NA, dim = c(1,2,3))

## ----NaN, eval = FALSE--------------------------------------------------------
# X = array(NaN, dim = c(1,2,3))
# # or
# X = array(0, dim = c(1,2,3))

## ----arrayfill, eval = FALSE--------------------------------------------------
# X <- AD(array(...))

## ----bytecompiler, message = FALSE--------------------------------------------
compiler::enableJIT(0)

