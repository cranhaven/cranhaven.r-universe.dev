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

library(LaMa)

## ----tpm----------------------------------------------------------------------
(Gamma = matrix(c(0.9, 0.1,
                  0.2, 0.8), nrow = 2, byrow = TRUE))

## ----stationary---------------------------------------------------------------
(delta = stationary(Gamma))

## ----data---------------------------------------------------------------------
# parameters
mu = c(0, 6)    # state-dependent means
sigma = c(2, 4) # state-dependent standard deviations
Gamma = matrix(c(0.95, 0.05, # transition probability matrix
                 0.15, 0.85), 
               nrow = 2, byrow = TRUE)
delta = stationary(Gamma) # stationary distribution

# simulation
n = 1000
set.seed(123)
s = rep(NA, n)
s[1] = sample(1:2, 1, prob = delta) # sampling first state from delta
for(t in 2:n){
  # drawing the next state conditional on the last one
  s[t] = sample(1:2, 1, prob = Gamma[s[t-1],]) 
}
# drawing the observation conditional on the states
x = rnorm(n, mu[s], sigma[s]) # independent given the state sequence

color = c("orange", "deepskyblue")
plot(x[1:200], bty = "n", pch = 20, ylab = "x", 
     col = color[s[1:200]])

## ----mllk---------------------------------------------------------------------
nll = function(par, x){
  # 1) Transition probability matrix
  Gamma = tpm(par[1:2]) # multinomial logistic link (softmax)
  # 2) Stationary distribution for this matrix
  delta = stationary(Gamma) # will be used as initial
  # Parameter transformations for unconstrained optimisation
  mu = par[3:4] # no transformation needed
  sigma = exp(par[5:6]) # strictly positive
  # 3) Calculating state-dependent densities outside the forward algorithm
  allprobs = matrix(1, length(x), 2)
  for(j in 1:2) allprobs[,j] = dnorm(x, mu[j], sigma[j])
  # 4) Run the forward algorithm
  -forward(delta, Gamma, allprobs)
}

## ----model, warning=FALSE-----------------------------------------------------
par = c(logitGamma = qlogis(c(0.05, 0.05)),
        mu = c(1,4),
        logsigma = c(log(1),log(3)))
# initial transformed parameters: not chosen too well
system.time(
  opt <- nlm(nll, par, x = x)
)

## ----visualization------------------------------------------------------------
estimate = opt$estimate
# transform parameters to natural scale
Gamma = tpm(estimate[1:2])
delta = stationary(Gamma) # stationary HMM
mu = estimate[3:4]
sigma = exp(estimate[5:6])

hist(x, prob = TRUE, bor = "white", breaks = 40, main = "")
curve(delta[1]*dnorm(x, mu[1], sigma[1]), add = TRUE, lwd = 2, col = "orange", n=500)
curve(delta[2]*dnorm(x, mu[2], sigma[2]), add = TRUE, lwd = 2, col = "deepskyblue", n=500)
curve(delta[1]*dnorm(x, mu[1], sigma[1])+delta[2]*dnorm(x, mu[2], sigma[2]),
      add = TRUE, lwd = 2, lty = "dashed", n=500)
legend("topright", col = c(color, "black"), lwd = 2, bty = "n",
       lty = c(1,1,2), legend = c("state 1", "state 2", "marginal"))

## ----states-------------------------------------------------------------------
allprobs = matrix(1, length(x), 2)
for(j in 1:2) allprobs[,j] = dnorm(x, mu[j], sigma[j])

states = viterbi(delta, Gamma, allprobs)

plot(x, pch = 20, bty = "n", col = color[states])
legend("topright", pch = 20, legend = c("state 1", "state 2"), 
       col = color, box.lwd = 0)

## ----stateprobs---------------------------------------------------------------
probs = stateprobs(delta, Gamma, allprobs)

## ----pseudores----------------------------------------------------------------
pres = pseudo_res(x, # observations
                  "norm", # parametric distribution to use
                  list(mean = mu, sd = sigma), # parameters for that distribution
                  probs) # local state probabilities

# use the plotting method for LaMaResiduals
plot(pres, hist = TRUE)

