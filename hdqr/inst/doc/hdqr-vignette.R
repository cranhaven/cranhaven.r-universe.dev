## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(hdqr)
set.seed(315)
n <- 100
p <- 400
x <- matrix(data = rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
beta_star <- c(c(2, 1.5, 0.8, 1, 1.75, 0.75, 0.3), rep(0, (p - 7)))
eps <- rnorm(n, mean = 0, sd = 1)
y <- x %*% beta_star + eps

## ------------------------------------------------------------------------
lambda <- 10^(seq(1, -4, length.out=30))
lam2 <- 0.01
tau <- 0.5
fit <- hdqr(x, y, lambda=lambda, lam2=lam2, tau=tau)

## ------------------------------------------------------------------------
cv.fit <- cv.hdqr(x, y, lambda=lambda, tau=tau)

## ------------------------------------------------------------------------
nc.fit <- nc.hdqr(x=x, y=y, tau=tau, lambda=lambda, lam2=lam2, pen="scad")

## ------------------------------------------------------------------------
cv.nc.fit <- cv.nc.hdqr(y=y, x=x, tau=tau, lambda=lambda, lam2=lam2, pen="scad")

## ------------------------------------------------------------------------
coefs <- coef(fit, s = fit$lambda[3:5])
preds <- predict(fit, newx = tail(x), s = fit$lambda[3:5])
cv.coefs <- coef(cv.fit, s = c(0.02, 0.03))
cv.preds <- predict(cv.fit, newx = x[50:60, ], s = "lambda.min")
nc.coefs <- coef(nc.fit, s = nc.fit$lambda[3:5])
nc.preds <- predict(nc.fit, newx = tail(x), s = fit$lambda[3:5])
cv.nc.coefs <- coef(cv.nc.fit, s = c(0.02, 0.03))
cv.nc.preds <- predict(cv.nc.fit, newx = x[50:60, ], s = "lambda.min")

