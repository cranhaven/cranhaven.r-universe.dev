## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(logisticRR)
n <- 500
set.seed(1234)
X <- rbinom(n, 1, 0.3)
W <- rbinom(n, 1, 0.3); W[sample(1:n, n/3)] = 2
Z <- rep(0, n)
Z[sample(1:n, n/2)] <- "female"; Z <- ifelse(Z == 0, "male", Z)
dummyZ <- ifelse(Z == "female", 1, 0)
Y <- rbinom(n, 1, plogis(X - W + 2*dummyZ))
dat <- as.data.frame(cbind(Y, X, W, Z))
dat$X <- as.numeric(dat$X); dat$X <- ifelse(dat$X == 2, 1, 0)
dat$Y <- as.numeric(dat$Y); dat$Y <- ifelse(dat$Y == 2, 1, 0)
dat$W <- as.factor(dat$W)
dat$Z <- as.factor(dat$Z)
head(dat)

