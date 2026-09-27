# mixture_bayessurvreg_helpers.R
# Internal helpers for Bayesian survival mixture worker.
# Keep these helpers in a file that is collated/sourced before mixture_bayessurvreg.R.

#' Validate survreg Dist String
#' @noRd
.validate_survreg_dist <- function(dist) {
 if (missing(dist) || !is.character(dist) || length(dist) != 1L) {
  stop("`dist` must be a single character string.", call. = FALSE)
 }
 tolower(dist)
}

#' Normalize survreg Response to (Time, Event)
#' @noRd
.normalize_surv_y <- function(y) {
 if (is.list(y) && all(c("time", "event") %in% names(y))) {
  time <- as.numeric(y$time)
  event <- as.integer(y$event)
 } else if (is.matrix(y) && ncol(y) >= 2L) {
  time <- as.numeric(y[, 1L])
  event <- as.integer(y[, 2L])
 } else {
  stop("`y` must be a 2-column matrix (time, event) or a list with time/event.", call. = FALSE)
 }

 if (any(!is.finite(time)) || any(time <= 0)) stop("Survival times must be positive and finite.", call. = FALSE)
 event[event != 0L] <- 1L
 list(time = time, event = event)
}

#' Generate Stan Code for a Two-Component Survival Mixture
#'
#' Creates a complete Stan program as a single string for a two-component mixture survival model
#' with both components of the same family (Gamma or Weibull). The Stan code includes
#' user-defined priors and any hyperparameters via \code{get_stan_definitions()}.
#'
#' @param components A length-2 character vector specifying the component distributions.
#'   Only \code{c("gamma","gamma")} or \code{c("weibull","weibull")} are supported.
#' @param priors A named list of prior specifications.
#'
#' @return A single character string containing a Stan program.
#'
#' @keywords internal
#' @noRd
generate_stan_surv <- function(components, priors = list()) {

 defs <- get_stan_definitions(priors)
 function_definitions <- defs$function_defs
 variable_definitions <- defs$variable_defs

 if (!is.character(components) || length(components) != 2L) {
  stop("`components` must be a length-2 character vector.", call. = FALSE)
 }

 if (identical(components, c("gamma","gamma"))) {

  stan_code <- paste(
   "functions {",
   function_definitions,
   "}",
   "data {",
   "  int<lower=1> N;",
   "  int<lower=1> K;",
   "  matrix[N, K] X;",
   "  vector[N] time;",
   "  int<lower=0,upper=1> event[N];",
   "}",
   "transformed data {",
   variable_definitions,
   "}",
   "parameters {",
   "  real<lower=0,upper=1> theta;",
   "  vector[K] beta1;",
   "  vector[K] beta2;",
   "  real<lower=0> phi1;",
   "  real<lower=0> phi2;",
   "}",
   "transformed parameters {",
   "  vector[N] eta1;",
   "  vector[N] eta2;",
   "  eta1 = X * beta1;",
   "  eta2 = X * beta2;",
   "}",
   "model {",
   "  // priors: separate intercept and slope priors",
   "  beta1[1] ~ ", priors$intercept1, ";",
   "  beta2[1] ~ ", priors$intercept2, ";",
   "  if (K > 1) {",
   "    beta1[2:K] ~ ", priors$beta1, ";",
   "    beta2[2:K] ~ ", priors$beta2, ";",
   "  }",
   "  theta ~ ", priors$theta, ";",
   "  phi1 ~ ", priors$phi1, ";",
   "  phi2 ~ ", priors$phi2, ";",
   "",
   "  // likelihood: right-censoring supported via survival function",
   "  for (n in 1:N) {",
   "    real lp1;",
   "    real lp2;",
   "    if (event[n] == 1) {",
   "      lp1 = gamma_lpdf(time[n] | phi1, phi1 / exp(eta1[n]));",
   "      lp2 = gamma_lpdf(time[n] | phi2, phi2 / exp(eta2[n]));",
   "    } else {",
   "      lp1 = gamma_lccdf(time[n] | phi1, phi1 / exp(eta1[n]));",
   "      lp2 = gamma_lccdf(time[n] | phi2, phi2 / exp(eta2[n]));",
   "    }",
   "    target += log_mix(theta, lp1, lp2);",
   "  }",
   "}",
   "generated quantities {",
   "  int<lower=1,upper=2> z[N];",
   "  for (n in 1:N) {",
   "    vector[2] lw;",
   "    if (event[n] == 1) {",
   "      lw[1] = log(theta) + gamma_lpdf(time[n] | phi1, phi1 / exp(eta1[n]));",
   "      lw[2] = log1m(theta) + gamma_lpdf(time[n] | phi2, phi2 / exp(eta2[n]));",
   "    } else {",
   "      lw[1] = log(theta) + gamma_lccdf(time[n] | phi1, phi1 / exp(eta1[n]));",
   "      lw[2] = log1m(theta) + gamma_lccdf(time[n] | phi2, phi2 / exp(eta2[n]));",
   "    }",
   "    z[n] = categorical_rng(softmax(lw));",
   "  }",
   "}",
   sep = "\n"
  )

  return(stan_code)
 }

 if (identical(components, c("weibull","weibull"))) {

  stan_code <- paste(
   "functions {",
   function_definitions,
   "}",
   "data {",
   "  int<lower=1> N;",
   "  int<lower=1> K;",
   "  matrix[N, K] X;",
   "  vector[N] time;",
   "  int<lower=0,upper=1> event[N];",
   "}",
   "transformed data {",
   variable_definitions,
   "}",
   "parameters {",
   "  real<lower=0,upper=1> theta;",
   "  vector[K] beta1;",
   "  vector[K] beta2;",
   "  real<lower=0> shape1;",
   "  real<lower=0> shape2;",
   "  real<lower=0> scale1;",
   "  real<lower=0> scale2;",
   "}",
   "transformed parameters {",
   "  vector[N] eta1;",
   "  vector[N] eta2;",
   "  eta1 = X * beta1;",
   "  eta2 = X * beta2;",
   "}",
   "model {",
   "  // priors: separate intercept and slope priors",
   "  beta1[1] ~ ", priors$intercept1, ";",
   "  beta2[1] ~ ", priors$intercept2, ";",
   "  if (K > 1) {",
   "    beta1[2:K] ~ ", priors$beta1, ";",
   "    beta2[2:K] ~ ", priors$beta2, ";",
   "  }",
   "  theta ~ ", priors$theta, ";",
   "  shape1 ~ ", priors$shape1, ";",
   "  shape2 ~ ", priors$shape2, ";",
   "  scale1 ~ ", priors$scale1, ";",
   "  scale2 ~ ", priors$scale2, ";",
   "",
   "  for (n in 1:N) {",
   "    real lp1;",
   "    real lp2;",
   "    if (event[n] == 1) {",
   "      lp1 = weibull_lpdf(time[n] | shape1, scale1 * exp(eta1[n]));",
   "      lp2 = weibull_lpdf(time[n] | shape2, scale2 * exp(eta2[n]));",
   "    } else {",
   "      lp1 = weibull_lccdf(time[n] | shape1, scale1 * exp(eta1[n]));",
   "      lp2 = weibull_lccdf(time[n] | shape2, scale2 * exp(eta2[n]));",
   "    }",
   "    target += log_mix(theta, lp1, lp2);",
   "  }",
   "}",
   "generated quantities {",
   "  int<lower=1,upper=2> z[N];",
   "  for (n in 1:N) {",
   "    vector[2] lw;",
   "    if (event[n] == 1) {",
   "      lw[1] = log(theta) + weibull_lpdf(time[n] | shape1, scale1 * exp(eta1[n]));",
   "      lw[2] = log1m(theta) + weibull_lpdf(time[n] | shape2, scale2 * exp(eta2[n]));",
   "    } else {",
   "      lw[1] = log(theta) + weibull_lccdf(time[n] | shape1, scale1 * exp(eta1[n]));",
   "      lw[2] = log1m(theta) + weibull_lccdf(time[n] | shape2, scale2 * exp(eta2[n]));",
   "    }",
   "    z[n] = categorical_rng(softmax(lw));",
   "  }",
   "}",
   sep = "\n"
  )

  return(stan_code)
 }

 stop("Only Gamma-Gamma and Weibull-Weibull components are supported for survival.", call. = FALSE)
}
