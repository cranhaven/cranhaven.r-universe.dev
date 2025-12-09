## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "",
  fig.width = 7,
  fig.height = 5
)

library(kDGLM)
# devtools::load_all()

## ----eval=FALSE, include=TRUE-------------------------------------------------
# Normal(mu, V = NA, Tau = NA, Sd = NA, data)

## -----------------------------------------------------------------------------
level <- polynomial_block(mu = 1, D = 0.95, order = 2)
season <- harmonic_block(mu = 1, period = 12, D = 0.975)

outcome <- Normal(
  mu = "mu", V = 6e-3,
  data = c(log(AirPassengers))
)
fitted.model <- fit_model(level, season, outcome)
plot(fitted.model, plot.pkg = "base")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# Normal(mu, V = NA, Tau = NA, Sd = NA, data)

## -----------------------------------------------------------------------------
structure <- polynomial_block(mu = 1, D = 0.95) +
  polynomial_block(V = 1, D = 0.95)

outcome <- Normal(mu = "mu", V = "V", data = cornWheat$corn.log.return[1:500])
fitted.model <- fit_model(structure, outcome)
plot(fitted.model, plot.pkg = "base")

## ----results='hide'-----------------------------------------------------------
# Bivariate Normal case
structure <- (polynomial_block(mu = 1, D = 0.95) +
  polynomial_block(log.V = 1, D = 0.95)) * 2 +
  polynomial_block(atanh.rho = 1, D = 0.95)

outcome <- Normal(
  mu = c("mu.1", "mu.2"),
  V = matrix(c("log.V.1", "atanh.rho", "atanh.rho", "log.V.2"), 2, 2),
  data = cornWheat[1:500, c(4, 5)]
)
fitted.model <- fit_model(structure, outcome)

## ----results='hide'-----------------------------------------------------------
plot(fitted.model, plot.pkg = "base")

## ----results='hide'-----------------------------------------------------------
plot(fitted.model, linear.predictors = "atanh.rho", plot.pkg = "base")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# Poisson(lambda, data, offset = data^0)

## ----results='hide'-----------------------------------------------------------
data <- c(AirPassengers)

level <- polynomial_block(rate = 1, order = 2, D = 0.95)
season <- harmonic_block(rate = 1, period = 12, order = 2, D = 0.975)

outcome <- Poisson(lambda = "rate", data = data)

fitted.data <- fit_model(level, season,
  AirPassengers = outcome
)
plot(fitted.data, plot.pkg = "base")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# Gamma(phi = NA, mu = NA, alpha = NA, beta = NA, sigma = NA, data = , offset = data^0)

## ----results='hide'-----------------------------------------------------------
structure <- polynomial_block(mu = 1, D = 0.95)
Y <- (cornWheat$corn.log.return[1:500] - mean(cornWheat$corn.log.return[1:500]))**2

outcome <- Gamma(phi = 0.5, mu = "mu", data = Y)
fitted.data <- fit_model(structure, outcome)
plot(fitted.data, plot.pkg = "base")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# Multinom(p, data, offset = data^0)

## ----results='hide'-----------------------------------------------------------
# Multinomial case
structure <- (
  polynomial_block(p = 1, order = 2, D = 0.95) +
    harmonic_block(p = 1, period = 12, D = 0.975) +
    noise_block(p = 1, R1 = 0.1) +
    regression_block(p = chickenPox$date >= as.Date("2013-09-01"))
  # Vaccine was introduced in September of 2013
) * 4

outcome <- Multinom(p = structure$pred.names, data = chickenPox[, c(2, 3, 4, 6, 5)])
fitted.data <- fit_model(structure, chickenPox = outcome)
summary(fitted.data)
plot(fitted.data, plot.pkg = "base")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# structure <- polynomial_block(mu.1 = 1, mu.2 = 1, order = 2, D = 0.95) + # Common factor
#   harmonic_block(mu.2 = 1, period = 12, order = 2, D = 0.975) + # Seasonality for Series 2
#   polynomial_block(mu.2 = 1, order = 1, D = 0.95) + # Local level for Series 2
#   noise_block(mu = 1) * 2 # Overdispersion for both Series
# 
# fitted.model <- fit_model(structure,
#   Adults = Poisson(lambda = "mu.1", data = chickenPox[, 5]),
#   Infants = Poisson(lambda = "mu.2", data = chickenPox[, 2])
# )
# 
# plot(fitted.model)

## -----------------------------------------------------------------------------
structure <- polynomial_block(mu = 1, order = 2, D = 0.95) +
  harmonic_block(mu = 1, period = 12, order = 2, D = 0.975) +
  noise_block(mu = 1) + polynomial_block(p = 1, D = 0.95) * 2

outcome1 <- Poisson(lambda = "mu", data = rowSums(chickenPox[, c(2, 3, 5)]))
outcome2 <- Multinom(p = c("p.1", "p.2"), data = chickenPox[, c(2, 3, 5)])

fitted.model <- fit_model(structure, Total = outcome1, Proportions = outcome2)

plot(fitted.model, plot.pkg = "base")

## ----eval=FALSE, include=FALSE------------------------------------------------
# rmarkdown::render("vignettes/vignette.Rmd")

