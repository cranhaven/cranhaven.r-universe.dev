#' Data generated from a constant velocity (or continuous white noise acceleration, CWNA) model for 20 time steps.
#'
#' @format 20 observations of 4 variables.
"cwna_data"

#' The output of a bootstrap particle filter on the `cwna_data`. The output consists of 100 particles over 20 time steps.
#'
#' @format 4000 observations of 13 variables.
"sir_cwna_model"

#' The output of an SMC sampler where the initial distribution is a Gaussian and the final target is a mixture of Gaussians. 25 particles were used, with an adaptive method to determine the sequence of targets, and a Metropolis-Hastings move to move the particles at each step.
#'
#' @format 175 observations of 13 variables.
"mixture_25_particles"

#' 10000 simulations from a stochastic Lotka-Volterra model, assigned weights according to a Gaussian approximate Bayesian computation kernel with tolerance equal to 50.
#'
#' @format 320000 observations of 15 variables.
"lv_output"
