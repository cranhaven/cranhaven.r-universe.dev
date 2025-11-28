## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example------------------------------------------------------------------
library(ergmito)
data(fivenets)
model_object <- ergmito_formulae(fivenets ~ edges + ttriad)

# Printing the model object
model_object

# Printing the log-likelihood function
model_object$loglik

## ----looking-at-the-components------------------------------------------------
# The vectors of weights
str(model_object$stats_weights)

# The matrices of the sufficient statistics
str(model_object$stats_statmat)

# The target statistic
model_object$target_stats

## -----------------------------------------------------------------------------
# Analyzing the model
model_object <- ergmito_formulae(fivenets ~ edges + nodematch("female")) 

# Defining the logposterior
logposterior <- function(p) {
  model_object$loglik(params = p) + 
  sum(dnorm(p, mean = c(-1,1), sd = sqrt(2), log = TRUE))
}
 

## -----------------------------------------------------------------------------
# Loading the required R packages
library(fmcmc)
library(coda)

# Is it working?
logposterior(c(-1, 1))

# Now, calling the MCMC function from the fmcmc R package
ans <- MCMC(
  fun     = logposterior,
  initial = c(0, 0),
  # 5,000 steps sampling one of every ten iterations
  nsteps  = 5000,
  thin    = 10,
  # We are using a normal transition kernel with .5 scale and updates are done
  # one variable at a time in a random order
  kernel = kernel_normal(scale = .5, scheme = "random")
  )

## ----looking-at-the-output, fig.height=7, fig.width=7-------------------------
plot(ans)
summary(ans)

## -----------------------------------------------------------------------------
summary(ergmito(fivenets ~ edges + nodematch("female")))

