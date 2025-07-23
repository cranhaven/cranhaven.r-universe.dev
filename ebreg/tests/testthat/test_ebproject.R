context("Test ebreg function.")

dcomplex <- function(x, n, p, a, b, log=TRUE) {

  o <- -x * (log(b) + a * log(p)) + log(x <= n)
  if(!log) o <- exp(o)
  return(o)

}

test_that('the right things are NULL when pred is FALSE', {
  n=100
  p=200
  r=0.5
  sig2 <- 1
  signal=1
  beta <- rep(1, 5)
  s0 <- length(beta)
  d <- 1
  log.f <- function(x) dcomplex(x, n, p, 0.05, 1)
  g <- function(i, j) r**(abs(i - j))
  R <- outer(1:p, 1:p, g)
  e <- eigen(R)
  sqR <- e$vectors %*% diag(sqrt(e$values)) %*% t(e$vectors)
  X <- matrix(rnorm(n * p), nrow=n, ncol=p) %*% sqR
  X.new <- matrix(rnorm(p), nrow=1, ncol=p) %*% sqR
  y <- as.numeric(X[, 1:s0] %*% beta[1:s0]) + sqrt(sig2) * rnorm(n)
  y.new <- as.numeric(X.new[,1:s0] %*% beta[1:s0]) + sqrt(sig2) * rnorm(1)
  o1 <- ebreg(y, X, X.new, FALSE, alpha=.99, gam=.005, NULL, FALSE, igpar=c(0.01, 4), log.f, M=5000, sample.beta=TRUE)

  expect_true( is.null(o1$ynew))
  expect_true( is.null(o1$ynew.mean))
  expect_true( is.null(o1$PI))

})

test_that('the right things are NULL when sample.beta is FALSE', {
  n=100
  p=200
  r=0.5
  signal=1
  sig2 <- 1
  beta <- rep(1, 5)
  s0 <- length(beta)
  d <- 1
  log.f <- function(x) dcomplex(x, n, p, 0.05, 1)
  g <- function(i, j) r**(abs(i - j))
  R <- outer(1:p, 1:p, g)
  e <- eigen(R)
  sqR <- e$vectors %*% diag(sqrt(e$values)) %*% t(e$vectors)
  X <- matrix(rnorm(n * p), nrow=n, ncol=p) %*% sqR
  X.new <- matrix(rnorm(p), nrow=1, ncol=p) %*% sqR
  y <- as.numeric(X[, 1:s0] %*% beta[1:s0]) + sqrt(sig2) * rnorm(n)
  y.new <- as.numeric(X.new[,1:s0] %*% beta[1:s0]) + sqrt(sig2) * rnorm(1)
  o1 <- ebreg(y, X, X.new, FALSE, alpha=.99, gam=.005, NULL, FALSE, igpar=c(0.01, 4), log.f, M=5000, pred=TRUE)

  expect_true( is.null(o1$beta))
  expect_true( is.null(o1$beta.mean))
  expect_true( is.null(o1$CI))

})

test_that('the right things are NULL when prior is TRUE', {
  n=100
  p=200
  r=0.5
  sig2 <- 1
  signal=1
  beta <- rep(1, 5)
  s0 <- length(beta)
  d <- 1
  log.f <- function(x) dcomplex(x, n, p, 0.05, 1)
  g <- function(i, j) r**(abs(i - j))
  R <- outer(1:p, 1:p, g)
  e <- eigen(R)
  sqR <- e$vectors %*% diag(sqrt(e$values)) %*% t(e$vectors)
  X <- matrix(rnorm(n * p), nrow=n, ncol=p) %*% sqR
  X.new <- matrix(rnorm(p), nrow=1, ncol=p) %*% sqR
  y <- as.numeric(X[, 1:s0] %*% beta[1:s0]) + sqrt(sig2) * rnorm(n)
  y.new <- as.numeric(X.new[,1:s0] %*% beta[1:s0]) + sqrt(sig2) * rnorm(1)
  o1 <- ebreg(y, X, X.new, FALSE, alpha=.99, gam=.005, NULL, TRUE, igpar=c(0.01, 4), log.f, M=5000, pred=TRUE)

  expect_true( is.null(o1$sig2))
})


test_that('returned values have the right size', {
  n=100
  p=200
  r=0.5
  M=5000
  signal=1
  sig2 <- 1
  beta <- rep(1, 5)
  s0 <- length(beta)
  d <- 1
  log.f <- function(x) dcomplex(x, n, p, 0.05, 1)
  g <- function(i, j) r**(abs(i - j))
  R <- outer(1:p, 1:p, g)
  e <- eigen(R)
  sqR <- e$vectors %*% diag(sqrt(e$values)) %*% t(e$vectors)
  X <- matrix(rnorm(n * p), nrow=n, ncol=p) %*% sqR
  X.new <- matrix(rnorm(p), nrow=1, ncol=p) %*% sqR
  y <- as.numeric(X[, 1:s0] %*% beta[1:s0]) + sqrt(sig2) * rnorm(n)
  y.new <- as.numeric(X.new[,1:s0] %*% beta[1:s0]) + sqrt(sig2) * rnorm(1)
  o1 <- ebreg(y, X, X.new, FALSE, alpha=.99, gam=.005, NULL, TRUE, igpar=c(0.01, 4), log.f, M=M, pred=TRUE, sample.beta=TRUE)

  expect_true(ncol(o1$beta)==p)
  expect_true(nrow(o1$beta)==M)
  expect_true(length(o1$beta.mean)==p)

})
