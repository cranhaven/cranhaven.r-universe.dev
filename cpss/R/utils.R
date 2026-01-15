# g_subdata ----
g_subdat <- function(dat, indices) {
  matrix(dat[indices, ], sum(indices), ncol(dat))
}

# g_param ----
g_param_mean <- function(dat, param.opt) {
  Sigma <- param.opt
  mu <- colMeans(dat)
  return(list(mu = mu, Sigma = Sigma))
}
g_param_var <- function(dat, param.opt) {
  mu <- param.opt
  temp <- sweep(dat, 2, mu)
  Sigma <- (t(temp) %*% temp) / nrow(temp)
  return(list(mu = mu, Sigma = Sigma))
}
#' @importFrom stats cov
g_param_meanvar <- function(dat, param.opt) {
  mu <- colMeans(dat)
  Sigma <- (nrow(dat) - 1) / nrow(dat) * cov(dat)
  return(list(mu = mu, Sigma = Sigma))
}
#' @importFrom stats family glm.fit
g_param_glm <- function(dat, param.opt) {
  family <- param.opt
  if (is.character(family)) {
    family <- get(family, mode = "function")
  }
  if (is.function(family)) {
    family <- family()
  }
  if (family$family == "binomial") {
    w <- dat[, 1] + dat[, 2]
    y <- dat[, 1] / w
    x <- dat[, -c(1, 2)]
  } else {
    y <- dat[, 1]
    x <- dat[, -1]
    w <- rep(1, length(y))
  }
  res <- glm.fit(x, y, weights = w, family = family)
  return(res)
}
#' @importFrom Rfast diri.nr2 gammamle beta.mle chisq.mle invgauss.mle
g_param_em <- function(dat, param.opt) {
  N <- NULL
  em <- param.opt$em
  if (em == "binom") {
    N <- param.opt$N
    param <- mean(dat) / N
  }
  if (em == "multinom") {
    N <- param.opt$N
    param <- colMeans(dat) / N
  }
  if (em %in% c("pois")) {
    param <- mean(dat)
  }
  if (em %in% c("exp", "geom")) {
    param <- 1 / mean(dat)
  }
  if (em == "diri") {
    param <- diri.nr2(dat, type = 2)$param
  }
  if (em == "gamma") {
    param <- gammamle(dat)$param
  }
  if (em %in% c("beta", "chisq", "invgauss")) {
    func.mle <- get(paste0(em, ".mle"), mode = "function")
    param <- func.mle(dat)$param
  }
  return(list(param = param, em = em, N = N))
}

# g_cost ----
#' @importFrom mvtnorm dmvnorm
g_cost_mvnorm <- function(dat, param) {
  sum(-2 * dmvnorm(dat, mean = param$mu, sigma = param$Sigma, log = TRUE))
}
#' @importFrom stats family
g_cost_glm <- function(dat, param) {
  family <- param$family
  if (family$family == "binomial") {
    w <- dat[, 1] + dat[, 2]
    y <- dat[, 1] / w
    x <- dat[, -c(1, 2)]
  } else {
    y <- dat[, 1]
    x <- dat[, -1]
    w <- rep(1, length(y))
  }
  eta <- drop(as.matrix(x) %*% as.matrix(param$coefficients))
  mu <- family$linkinv(eta)
  sum(family$dev.resids(y, mu, w))
  # param$deviance
}
#' @importFrom stats dbinom dmultinom dexp dpois dgeom dgamma dbeta dchisq
g_cost_em <- function(dat, param) {
  param_ <- param$param
  em <- param$em
  N <- param$N
  if (em == "binom") {
    loglik <- sum(dbinom(dat, N, param_, log = TRUE))
  }
  if (em == "multinom") {
    loglik <- sum(apply(dat, 1, dmultinom, N, param_, log = TRUE))
  }
  if (em %in% c("exp", "pois", "geom")) {
    func.d <- get(paste0("d", em), mode = "function")
    loglik <- sum(func.d(dat, param_, log = TRUE))
  }
  if (em == "diri") {
    loglik <- sum(log(gamma(sum(param_))) - sum(log(gamma(param_))) + rowSums(sweep(log(dat), 2, param_ - 1, "*")))
  }
  if (em %in% c("gamma", "beta")) {
    func.d <- get(paste0("d", em), mode = "function")
    loglik <- sum(func.d(dat, param_[1], param_[2], log = TRUE))
  }
  if (em == "chisq") {
    loglik <- sum(dchisq(dat, param_, log = TRUE))
  }
  if (em == "invgauss") {
    loglik <- sum(.5 * (log(param_[2] / (2 * pi)) - 3 * log(dat) - param_[2] / (param_[1]^2) * (dat - param_[1])^2 / dat))
  }
  return(-2 * loglik)
}
