## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "",
  fig.width = 7,
  fig.height = 5
)

library(kDGLM)
# devtools::load_all()
set.seed(13031998)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# polynomial_block(...,
#   order = 1, name = "Var.Poly",
#   D = 1, h = 0, H = 0,
#   a1 = 0, R1 = c(9, rep(1, order - 1)),
#   monitoring = c(TRUE, rep(FALSE, order - 1))
# )
# 
# # When used in a formula
# pol(order = 1, D = 0.95, a1 = 0, R1 = 9, name = "Var.Poly")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# mean_block <- polynomial_block(eta = 1, order = 1, name = "Mean")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# polynomial_block(eta = X, name = "Var X")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# mean_block <- polynomial_block(eta = 1, order = 1, name = "Mean", D = 0.95)

## ----echo=FALSE, results='hide'-----------------------------------------------
# Normal case
T <- 200
mu <- rnorm(T, 0, 0.5)
data <- rnorm(T, cumsum(mu))

level1 <- polynomial_block(
  mu1 = 1,
  D = 1,
  name = "Static mean"
)
level2 <- polynomial_block(
  mu2 = 1,
  D = 0.95,
  name = "Dynamic mean"
)
# Known variance
Static.mean <- Normal(mu = "mu1", V = 1, data = data)
Dynamic.mean <- Normal(mu = "mu2", V = 1, data = data)

fitted.data <- fit_model(level1, level2,
  Static.mean = Static.mean,
  Dynamic.mean = Dynamic.mean
)

plot(fitted.data, lag = -1, plot.pkg = "base")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# regression_block(...,
#   max.lag = 0,
#   zero.fill = TRUE,
#   name = "Var.Reg",
#   D = 1,
#   h = 0,
#   H = 0,
#   a1 = 0,
#   R1 = 9,
#   monitoring = rep(FALSE, max.lag + 1)
# )
# 
# # When used in a formula
# reg(X, max.lag = 0, zero.fill = TRUE, D = 0.95, a1 = 0, R1 = 9, name = "Var.Reg")

## ----include=FALSE------------------------------------------------------------
T <- 200
X <- rgamma(T, 20, 20 / 5)
sd_gamma <- 0.5 / (2 * sqrt(T))
gamma_coef <- 0.5 + cumsum(rnorm(T, 0, 0.1 / (2 * sqrt(T))))
data <- rpois(T, exp(gamma_coef * X))

## ----echo=TRUE----------------------------------------------------------------
regression <- regression_block(The_name_of_the_linear_predictor = X, D = 0.95)

outcome <- Poisson(lambda = "The_name_of_the_linear_predictor", data = data)

fitted.data <- fit_model(regression, outcome)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
plot(gamma_coef, type = "l", lty = 2, col = "red", ylim = c(0.45, 0.55), ylab = expression(theta[t]), xlab = "Time", main = expression(paste("Estimation of ", theta[t])))
lines(fitted.data$mts[1, ])
lines(fitted.data$mts[1, ] - 1.96 * sqrt(fitted.data$Cts[1, 1, ]), lty = 2)
lines(fitted.data$mts[1, ] + 1.96 * sqrt(fitted.data$Cts[1, 1, ]), lty = 2)
legend("topright", legend = c("True value", "Mean", "C.I. 95%"), lty = c(2, 1, 2), col = c("red", "black", "black"))

## ----eval=FALSE, include=TRUE-------------------------------------------------
# harmonic_block(
#   ...,
#   period,
#   order = 1,
#   name = "Var.Sazo",
#   D = 1,
#   h = 0,
#   H = 0,
#   a1 = 0,
#   R1 = 4,
#   monitoring = rep(FALSE, order * 2)
# )
# 
# # When used in a formula
# har(period, order = 1, D = 0.98, a1 = 0, R1 = 4, name = "Var.Sazo")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# mean_block <- harmonic_block(
#   eta = 1,
#   period = 12,
#   D = 0.975
# )

## ----include=FALSE------------------------------------------------------------
# Poisson case
T <- 60
w <- 2 * pi / 12
data <- rpois(T, exp(1.5 * cos(w * 1:T)))

## ----echo=FALSE, results='hide'-----------------------------------------------
season <- harmonic_block(rate = 1, period = 12, D = 0.975)

outcome <- Poisson(lambda = "rate", data = data)

fitted.data <- fit_model(season, outcome)

plot(fitted.data, lag = -1, plot.pkg = "base")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# TF_block(
#   ...,
#   order,
#   noise.var = NULL,
#   noise.disc = NULL,
#   pulse = 0,
#   name = "Var.AR",
#   AR.support = "free",
#   a1 = 0,
#   R1 = 9,
#   h = 0,
#   monitoring = TRUE,
#   D.coef = 1,
#   h.coef = 0,
#   H.coef = 0,
#   a1.coef = c(1, rep(0, order - 1)),
#   R1.coef = c(1, rep(0.25, order - 1)),
#   monitoring.coef = rep(FALSE, order),
#   a1.pulse = 0,
#   R1.pulse = 9,
#   D.pulse = 1,
#   h.pulse = 0,
#   H.pulse = 0,
#   monitoring.pulse = NA
# )
# 
# # When used in a formula
# TF(X, order = 1, noise.var = NULL, noise.disc = NULL, a1 = 0, R1 = 9, a1.coef = NULL, R1.coef = NULL, a1.pulse = 0, R1.pulse = 4, name = "Var.AR")
# 
# # Wrapper for the autoregressive structure
# AR(order = 1, noise.var = NULL, noise.disc = NULL, a1 = 0, R1 = 9, a1.coef = NULL, R1.coef = NULL, name = "Var.AR")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# mean_block <- TF_block(
#   eta = 1,
#   order = 1,
#   noise.var = 0.1
# )

## ----echo=FALSE, fig.height=10, fig.width=7-----------------------------------
T <- 200
phi <- 0.95
sigma2 <- 2
ht <- rep(NA, T)
ht_i <- 0
for (i in 1:T) {
  ht_i <- phi * ht_i + rnorm(1, 0, sqrt(sigma2))
  ht[i] <- ht_i
}
# plot(exp(ht))

data <- rgamma(T, 3 / 2, (3 / 2) / exp(ht))

volatility <- TF_block(
  eta = 1, order = 1,
  noise.var = sigma2
)

##########
fitted.data <- fit_model(
  volatility,
  Gamma(phi = 3 / 2, mu = "eta", data = data)
)

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 1))
x <- seq(fitted.data$mts[2, T] - 4 * sqrt(fitted.data$Cts[2, 2, T]),
  fitted.data$mts[2, T] + 4 * sqrt(fitted.data$Cts[2, 2, T]),
  l = 1000
)
fx <- dnorm(x, fitted.data$mts[2, T], sqrt(fitted.data$Cts[2, 2, T]))
plot(x, fx, main = "Posterior distribuition for the AR coefficient", type = "l", xlab = expression(phi), ylab = "Density")
lines(c(0.95, 0.95), c(0, max(fx) + 1), lty = 2)
legend("topright", legend = "True value", lty = c(2))

plot(ht, main = "Latent states estimation", xlab = "Time", ylab = expression(theta[t]))
lines(fitted.data$mts[1, ])
lines(fitted.data$mts[1, ] - 1.96 * sqrt(fitted.data$Cts[1, 1, ]), lty = 2)
lines(fitted.data$mts[1, ] + 1.96 * sqrt(fitted.data$Cts[1, 1, ]), lty = 2)
legend("bottomleft", legend = c("True states", "Estimated states"), lty = c(0, 1), pch = c(1, NA))
par(oldpar)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# regression_block(
#   mu = c(0, Y[-T]),
#   max.lag = k
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# noise_block(..., name = "Noise", D = 0.99, R1 = 1)
# 
# # When used in a formula
# noise(name = "Noise", D = 0.99, R1 = 0.1, H = 0)

## ----echo=FALSE---------------------------------------------------------------
set.seed(13031998)
T <- 200
mu <- 20 * (1:T) * (T:1) / (T**2)
data <- rpois(T, exp(mu + rnorm(T, 0, sqrt(0.1))))
ts.plot(data)

## ----results='hide'-----------------------------------------------------------
level <- polynomial_block(
  rate = 1,
  order = 3,
  D = 0.95
)

fitted.data <- fit_model(level,
  "Model 1" = Poisson(lambda = "rate", data = data)
)

plot(fitted.data, lag = 1, plot.pkg = "base")

## ----results='hide'-----------------------------------------------------------
level <- polynomial_block(
  mu = 1,
  order = 3,
  D = 0.95
)
noise <- noise_block(
  mu = 1
)

fitted.data <- fit_model(level, noise,
  "Model 2" = Poisson(lambda = "mu", data = data)
)

plot(fitted.data, lag = 1, plot.pkg = "base")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# block_1 <- ...
# .
# .
# .
# block_n <- ...
# 
# complete_structure <- block_superpos(block_1, ..., block_n)
# # or
# complete_structure <- block_1 + ... + block_n

## ----eval=FALSE, include=TRUE-------------------------------------------------
# poly_subblock <- polynomial_block(eta = 1, name = "Poly", D = 0.95)
# 
# regr_subblock <- regression_block(eta = X, name = "Regr", D = 0.95)
# 
# harm_subblock <- harmonic_block(eta = 1, period = 12, name = "Harm")
# 
# AR_subblock <- TF_block(eta = 1, order = 1, noise.var = 0.1, name = "AR")
# 
# complete_block <- poly_subblock + regr_subblock + harm_subblock + AR_subblock

## ----eval=FALSE, include=TRUE-------------------------------------------------
# polynomial_block(lambda1 = 1, lambda2 = 1, lambda3 = 1) # Common factor

## ----eval=FALSE, include=TRUE-------------------------------------------------
# polynomial_block(lambda1 = 1, order = 1) + # theta_1
#   polynomial_block(lambda2 = 1, order = 1) + # theta_2
#   polynomial_block(lambda3 = 1, order = 1) # theta_3

## ----eval=FALSE, include=TRUE-------------------------------------------------
# # Longer version of the previous code for the sake of clarity.
# # In general, when a block does not affect a particular linear predictor, that linear predictor should be ommited when creating the block.
# polynomial_block(lambda1 = 1, lambda2 = 0, lambda3 = 0, order = 1) + # theta_1
#   polynomial_block(lambda1 = 0, lambda2 = 1, lambda3 = 0, order = 1) + # theta_2
#   polynomial_block(lambda1 = 0, lambda2 = 0, lambda3 = 1, order = 1) # theta_3

## ----eval=FALSE, include=TRUE-------------------------------------------------
# polynomial_block(lambda1 = 1, order = 1) + # theta_1
#   polynomial_block(lambda2 = 1, order = 1) + # theta_2
#   polynomial_block(lambda3 = 1, order = 1) + # theta_3
#   polynomial_block(lambda1 = 1, lambda2 = 1, lambda3 = 1, order = 1) # theta_4: Common factor

## ----eval=FALSE, include=TRUE-------------------------------------------------
# #### Global level with linear growth ####
# polynomial_block(lambda1 = 1, lambda2 = 1, lambda3 = 1, D = 0.95, order = 2) +
#   #### Local variables for lambda1 ####
#   polynomial_block(lambda1 = 1, order = 1) +
#   regression_block(lambda1 = X1, max.lag = 3) +
#   harmonic_block(lambda1 = 1, period = 12, D = 0.98) +
#   #### Local variables for lambda2 ####
#   polynomial_block(lambda2 = 1, order = 1) +
#   TF_block(lambda2 = 1, pulse = X2, order = 1, noise.disc = 1) +
#   harmonic_block(lambda2 = 1, period = 12, D = 0.98, order = 2) +
#   #### Local variables for lambda3 ####
#   polynomial_block(lambda3 = 1, order = 1) +
#   TF_block(lambda3 = 1, order = 2, noise.disc = 0.9) +
#   regression_block(lambda3 = X3, D = 0.95)

## -----------------------------------------------------------------------------
base.block <- polynomial_block(eta = 1, name = "Poly", D = 0.95, order = 1)

# final.block <- block_mult(base.block, 4)
# or
# final.block <- base.block * 4
# or
final.block <- 4 * base.block

## -----------------------------------------------------------------------------
final.block$pred.names

## ----eval=FALSE, include=TRUE-------------------------------------------------
# final.block <- block_rename(final.block, c("Matthew", "Mark", "Luke", "John"))
# final.block$pred.names

## ----eval=FALSE, include=TRUE-------------------------------------------------
# phi_block <- polynomial_block(phi = 1, order = 1)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# theta_block <- polynomial_block(lambda = "phi", order = 1)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# polynomial_block(eta1 = 1, order = 1) +
#   polynomial_block(eta2 = "eta1", order = 1) +
#   polynomial_block(eta3 = "eta2", order = 1)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# joint_prior(block, var.index = 1:block$n, a1 = block$a1[var.index], R1 = block$R1[var.index, var.index])

## ----eval=FALSE, include=TRUE-------------------------------------------------
# polynomial_block(mu = 1, order = 2, D = 0.95) |>
#   block_mult(5) |>
#   joint_prior(a1 = prior.mean, R1 = prior.var)
# # assuming the objects prior.mean and prior.var are defined.

