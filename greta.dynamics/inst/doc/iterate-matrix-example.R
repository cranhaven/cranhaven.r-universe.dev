## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(comment = "#>",
                      eval = greta:::check_tf_version("message"))

## ----setup--------------------------------------------------------------------
library(greta.dynamics)
greta_sitrep()

## -----------------------------------------------------------------------------
# simulate from a probabilistic 4-stage transition matrix model
k <- 4

# component variables
# survival probability for all stages
survival <- uniform(0, 1, dim = k)
# conditional (on survival) probability of staying in a stage
stasis <- c(uniform(0, 1, dim = k - 1), 1)
# marginal probability of staying/progressing
stay <- survival * stasis
progress <- (survival * (1 - stay))[1:(k - 1)]
# recruitment rate for the largest two stages
recruit <- exponential(c(3, 5))

## -----------------------------------------------------------------------------
# combine into a matrix:
tmat <- zeros(k, k)
diag(tmat) <- stay
progress_idx <- row(tmat) - col(tmat) == 1
tmat[progress_idx] <- progress
tmat[1, k - (1:0)] <- recruit

## -----------------------------------------------------------------------------
# analyse this to get the intrinsic growth rate and stable state
iterations <- iterate_matrix(tmat)
iterations$lambda
iterations$stable_distribution
iterations$all_states

## -----------------------------------------------------------------------------
# Can also do this simultaneously for a collection of transition matrices
k <- 2
n <- 10
survival <- uniform(0, 1, dim = c(n, k))
stasis <- cbind(uniform(0, 1, dim = n), rep(1, n))
stay <- survival * stasis
progress <- (survival * (1 - stasis))[, 1]
recruit_rate <- 1 / seq(0.1, 5, length.out = n)
recruit <- exponential(recruit_rate, dim = n)
tmats <- zeros(10, 2, 2)
tmats[, 1, 1] <- stasis[, 1]
tmats[, 2, 2] <- stasis[, 2]
tmats[, 2, 1] <- progress
tmats[, 1, 2] <- recruit

## -----------------------------------------------------------------------------
iterations <- iterate_matrix(tmats)
iterations$lambda
iterations$stable_distribution
dim(iterations$all_states)
iterations$all_states[, , 1]
iterations$all_states[, , 100]

