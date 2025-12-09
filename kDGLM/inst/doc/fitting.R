## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "",
  fig.width = 7,
  fig.height = 5
)

library(kDGLM)
# devtools::load_all()

## -----------------------------------------------------------------------------
level <- polynomial_block(rate = 1, order = 2, D = 0.95)
season <- harmonic_block(rate = 1, period = 12, order = 2, D = 0.975)

outcome <- Poisson(lambda = "rate", data = c(AirPassengers))

fitted.model <- fit_model(
  level, season, # Strucuture
  AirPassengers = outcome
) # outcome

## ----eval=FALSE, include=TRUE-------------------------------------------------
# plot.fitted_dlm(model, pred.cred = 0.95, lag = 1, cutoff = floor(model$t / 10), plot.pkg = "auto")

## -----------------------------------------------------------------------------
plot(fitted.model, plot.pkg = "base")

## -----------------------------------------------------------------------------
summary(fitted.model)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# coef(object, t.eval = seq_len(object$t), lag = -1, pred.cred = 0.95, eval.pred = FALSE, eval.metric = FALSE, ...)

## -----------------------------------------------------------------------------
fitted.coef <- coef(fitted.model)
plot(fitted.coef, plot.pkg = "base")

## -----------------------------------------------------------------------------
plot(fitted.coef, "Var.Poly.Level", plot.pkg = "base")

## -----------------------------------------------------------------------------
plot(fitted.coef, "rate", plot.pkg = "base")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# forecast(object,
#   t = 1,
#   plot = ifelse(requireNamespace("plotly", quietly = TRUE), "plotly", ifelse(requireNamespace("ggplot2", quietly = TRUE), "ggplot2", "base")),
#   pred.cred = 0.95,
#   ...
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# forecast(fitted.model, t = 20, plot = "base")$plot

## ----error=TRUE,warning=TRUE--------------------------------------------------
try({
structure <-
  polynomial_block(p = 1, order = 2, D = 0.95) +
  harmonic_block(p = 1, period = 12, D = 0.975) +
  noise_block(p = 1, R1 = 0.1) +
  regression_block(
    p = chickenPox$date >= as.Date("2013-09-1"),
    # Vaccine was introduced in September of 2013
    name = "Vaccine"
  )

outcome <- Multinom(p = c("p.1", "p.2"), data = chickenPox[, c(2, 3, 5)])
fitted.model <- fit_model(structure * 2, chickenPox = outcome)

forecast(fitted.model, t = 24, plot = "base") # Missing extra arguments
})

## ----results='hide'-----------------------------------------------------------
forecast(fitted.model,
  t = 24,
  Vaccine.1.Covariate = rep(TRUE, 24), # Extra argument for covariate 1
  Vaccine.2.Covariate = rep(TRUE, 24), plot = "base"
) # Extra argument for covariate 2

## ----eval=FALSE, include=TRUE-------------------------------------------------
# update.fitted_dlm(object, ...)

## -----------------------------------------------------------------------------
level <- polynomial_block(rate = 1, order = 2, D = 0.95)
season <- harmonic_block(rate = 1, period = 12, order = 2, D = 0.975)
# Omitting the last 44 observations
outcome <- Poisson(lambda = "rate", data = c(AirPassengers)[1:100])
fitted.model <- fit_model(
  level, season, # Strucuture
  AirPassengers = outcome
) # outcome
updated.fit <- update(fitted.model,
  AirPassengers = list(data = c(AirPassengers)[101:144])
)

## -----------------------------------------------------------------------------
data <- c(AirPassengers)
# Adding an artificial change, so that we can make an intervention on the data at that point
# Obviously, one should NOT change their own data.
data[60:144] <- data[60:144] + 100

level <- polynomial_block(rate = 1, order = 2, D = 0.95)
season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)

# Reducing the discount factor so that the model can capture the expected change.
level <- level |> intervention(time = 60, D = 0.005, var.index = 1)
# Comment the line above to see the fit without the intervention

fitted.model <- fit_model(level, season,
  AirPassengers = Poisson(lambda = "rate", data = data)
)

plot(fitted.model, plot.pkg = "base")

## -----------------------------------------------------------------------------
data <- c(AirPassengers)
# Adding an artificial change, so that we can make an intervention on the data at that point
# Obviously, one should NOT change their own data.
data[60:144] <- data[60:144] + 100

level <- polynomial_block(rate = 1, order = 2, D = 0.95)
season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)

fitted.model <- fit_model(level, season,
  AirPassengers = Poisson(lambda = "rate", data = data),
  p.monit = 0.05
)

plot(fitted.model, plot.pkg = "base")

## -----------------------------------------------------------------------------
level <- polynomial_block(rate = 1, order = 2, D = "D1")

## -----------------------------------------------------------------------------
summary(level)

## ----error=TRUE,warning=TRUE--------------------------------------------------
try({
# D1 is missing
season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
outcome <- Poisson(lambda = "rate", data = c(AirPassengers))
fitted.model <- fit_model(level, season, AirPassengers = outcome)
})

## -----------------------------------------------------------------------------
# D1 is set within the fit method
season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
outcome <- Poisson(lambda = "rate", data = c(AirPassengers))
fitted.model <- fit_model(level, season, AirPassengers = outcome, D1 = 0.95)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# fit.dlm_block(...,
#   smooth = TRUE, p.monit = NA,
#   condition = "TRUE", metric = "log.like",
#   pred.cred = 0.95, metric.cutoff = NA, lag = 1
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# level <- polynomial_block(rate = 1, order = 2, D = "D.level")
# season <- harmonic_block(
#   rate = "sazo.effect", period = 12,
#   order = 2, D = "D.sazo"
# )
# 
# outcome <- Poisson(lambda = "rate", data = c(AirPassengers))
# 
# fit_model(level, season, outcome,
#   sazo.effect = c(0, 1),
#   D.level = seq(0.8, 1, l = 11),
#   D.sazo = seq(0.95, 1, l = 11),
#   condition = "sazo.effect==1 | D.sazo==1"
# )$search.data |> head()

## ----eval=FALSE, include=TRUE-------------------------------------------------
# # Creating a block for each order
# level <- polynomial_block(rate = "pol.ord.1", order = 1, D = 0.95) +
#   polynomial_block(rate = "pol.ord.2", order = 2, D = 0.95) +
#   polynomial_block(rate = "pol.ord.3", order = 3, D = 0.95) +
#   polynomial_block(rate = "pol.ord.4", order = 4, D = 0.95)
# season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
# 
# outcome <- Poisson(lambda = "rate", data = c(AirPassengers))
# 
# fit_model(level, season, outcome,
#   # Each block can be present (1) or absent (0).
#   pol.ord.1 = c(0, 1), pol.ord.2 = c(0, 1),
#   pol.ord.3 = c(0, 1), pol.ord.4 = c(0, 1),
#   condition = "pol.ord.1+pol.ord.2+pol.ord.3+pol.ord.4==1"
#   # Only test combinations with exactly one polynomial block.
# )$search.data |> head()

## ----eval=FALSE, include=TRUE-------------------------------------------------
# simulate(fitted.model, 5000)

## -----------------------------------------------------------------------------
H.range <- seq.int(0, 2, l = 100)
log.like.H <- seq_along(H.range)
log.prior.H <- dlnorm(H.range, 0, 1, log = TRUE)
for (i in seq_along(H.range)) {
  level <- polynomial_block(rate = 1, order = 2, H = H.range[i])
  season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
  # Using only 10 observations, for the sake of a pretty plot. For this particular application, the posterior density of H rapidly becomes highly consentrated in a single value.
  outcome <- Poisson(lambda = "rate", data = c(AirPassengers)[1:10])
  fitted.model <- fit_model(level, season, outcome)
  log.like.H[i] <- eval_dlm_norm_const(fitted.model)
}
log.post.H <- log.prior.H + log.like.H
post.H <- exp(log.post.H - max(log.post.H))
plot(H.range, post.H, type = "l", xlab = "H", ylab = "f(H|y)")

