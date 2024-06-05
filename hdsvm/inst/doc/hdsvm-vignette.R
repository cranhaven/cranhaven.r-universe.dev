## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(hdsvm)
set.seed(315)
n <- 100
p <- 400
x1 <- matrix(rnorm(n / 2 * p, -0.25, 0.1), n / 2)
x2 <- matrix(rnorm(n / 2 * p, 0.25, 0.1), n / 2)
x <- rbind(x1, x2)
beta <- 0.1 * rnorm(p)
prob <- plogis(c(x %*% beta))
y <- 2 * rbinom(n, 1, prob) - 1

## ------------------------------------------------------------------------
lambda <- 10^(seq(1, -4, length.out=30))
lam2 <- 0.01
fit <- hdsvm(x, y, lambda=lambda, lam2=lam2)

## ------------------------------------------------------------------------
cv.fit <- cv.hdsvm(x, y, lambda=lambda)

## ------------------------------------------------------------------------
nc.fit <- nc.hdsvm(x=x, y=y, lambda=lambda, lam2=lam2, pen="scad")

## ------------------------------------------------------------------------
cv.nc.fit <- cv.nc.hdsvm(y=y, x=x, lambda=lambda, lam2=lam2, pen="scad")

## ------------------------------------------------------------------------
coefs <- coef(fit, s = fit$lambda[3:5])
preds <- predict(fit, newx = tail(x), s = fit$lambda[3:5])
cv.coefs <- coef(cv.fit, s = c(0.02, 0.03))
cv.preds <- predict(cv.fit, newx = x[50:60, ], s = "lambda.min")
nc.coefs <- coef(nc.fit, s = nc.fit$lambda[3:5])
nc.preds <- predict(nc.fit, newx = tail(x), s = fit$lambda[3:5])
cv.nc.coefs <- coef(cv.nc.fit, s = c(0.02, 0.03))
cv.nc.preds <- predict(cv.nc.fit, newx = x[50:60, ], s = "lambda.min")

