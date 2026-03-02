## Copyright (C) 2024 Ching-Chuan Chen
##
## This file is part of RcppLbfgsBlaze.
##
## RcppLbfgsBlaze is free software: you can redistribute it and/or modify it
## under the terms of the MIT License. You should have received
## a copy of MIT License along with RcppLbfgsBlaze.
## If not, see https://opensource.org/license/mit.

suppressPackageStartupMessages({
  require(stats)
  require(microbenchmark)
  require(RcppLbfgsBlaze)
})

## define different versions of lm
exprs <- list()

# default version used in glm.fit()
exprs$glm.fit <- expression(stats::glm.fit(X1, y, family = binomial()))

# To use R optim
exprs$optim <- expression(optim(rep(0, ncol(X1)), likelihood, gradient, X = X1, y = y, method = "L-BFGS"))

# R optim with cppFunction
if (suppressMessages(require(RcppArmadillo, quietly = TRUE))) {
    likelihood_arma <- Rcpp::cppFunction("double likelihood_cpp(const arma::vec& par, const arma::mat& X, const arma::vec& y) {
      arma::vec eta = arma::clamp(X * par, -30, 30);
      arma::vec phat = 1/(1 + arma::exp(-eta));
      return -arma::dot(eta, y) - arma::sum(arma::log(1 - phat));
    }", depends = "RcppArmadillo")

    gradient_arma <- Rcpp::cppFunction("arma::vec gradient_cpp(const arma::vec& par, const arma::mat& X, const arma::vec& y) {
      arma::vec phat = 1/(1 + arma::exp(-arma::clamp(X * par, -30, 30)));
      return X.t() * (phat - y);
    }", depends = "RcppArmadillo")

	exprs$optim_arma <- expression(optim(rep(0, ncol(X1)), likelihood_arma, gradient_arma, X = X1, y = y, method = "L-BFGS"))
}

# glmnet
if (suppressMessages(require(glmnet, quietly = TRUE))) {
	exprs$glmnet <- expression(glmnet(X, y, lambda = 0, family = "binomial", standardize=FALSE))
}

# lbfgs
if (suppressMessages(require(lbfgs, quietly = TRUE))) {
	exprs$lbfgs <- expression(lbfgs(
	  likelihood, gradient, rep(0, ncol(X1)), X = X1, y = y, m = 8, past = 4,
	  linesearch_algorithm = "LBFGS_LINESEARCH_BACKTRACKING",
	  invisible = TRUE
	))

	if (suppressMessages(require(RcppArmadillo, quietly = TRUE))) {
		exprs$lbfgs_arma <- expression(lbfgs(
		  likelihood_arma, gradient_arma, rep(0, ncol(X1)), X = X1, y = y, m = 8, past = 4,
		  linesearch_algorithm = "LBFGS_LINESEARCH_BACKTRACKING",
		  invisible = TRUE
		))
	}
}

# RcppNumerical
if (suppressMessages(require(RcppNumerical, quietly = TRUE))) {
	exprs$RcppNumerical <- expression(fastLR(X1, as.numeric(y)))
}

# RcppLbfgsBlaze
exprs$RcppLbfgsBlaze <- expression(fastLogisticModel(X1, as.numeric(y)))

do_bench <- function(n = 1e4L, p = 100L, non_zero_pro = 0.06, nrep = 20L) {
  nzc <- ceiling(p*non_zero_pro/2)
  X <- matrix(rnorm(n * p), n, p)
  beta <- c(rnorm(nzc, 2, 0.4), rnorm(nzc, -2, 0.4), rep(0, p - 2 * nzc))
  y <- sapply(1/(1 + exp(-2 - X %*% beta)), function(p) rbinom(1, 1, p), USE.NAMES = FALSE)
  X1 <- cbind(1, X)

  likelihood <- function(par, X, y) {
    eta <- pmin(pmax(X %*% par, -30), 30)
    phat <- 1/(1+exp(-eta))
    -crossprod(eta, y) - sum(log(1 - phat))
  }

  gradient <- function(par, X, y) {
    phat <- 1/(1+exp(-pmin(pmax(X %*% par, -30), 30)))
    t(X) %*% (phat - y)
  }

  cat("logistic model fitting benchmark for n = ", n, ", p = ", p, " and non-zero p = ", nzc*2, ": nrep = ", nrep, "\n", sep="")
  microbenchmark(list = do.call(c, exprs), times = nrep)
}

print(do_bench())
