## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sp)
library(gstat)
library(DSSP)
library(ggplot2)

data("meuse.all")
data("meuse")

## -----------------------------------------------------------------------------
meuse.train <- meuse.all[1:155, ]
meuse.valid <- meuse.all[156:164, ]
coordinates(meuse.train) <- ~ x + y
coordinates(meuse.valid) <- ~ x + y

## -----------------------------------------------------------------------------
meuse.fit <- DSSP(
  formula = log(zinc) ~ 1, data = meuse.train, N = 10000,
  pars = c(0.001, 0.001), log_prior = function(x) -2 * log(1 + x)
)

## ---- fig.align="center", fig.width=5.5, fig.height=4.25----------------------
meuse.train$Yhat <- rowMeans(exp(predict(meuse.fit)))

ggplot(as.data.frame(meuse.train), aes(Yhat, zinc)) +
  geom_point(size = 3) +
  geom_abline() +
  labs(
    x = "Smoothed Values", y = "Observed Values",
    title = "Smoothed vs. Observed Values"
  )

## ---- fig.height=3, fig.width=3.475, fig.show='hold'--------------------------
ggplot(data.frame(x = meuse.fit$eta)) +
  geom_density(aes(x = x)) +
  labs(
    x = expression(eta), y = "posterior density",
    title = expression("Posterior Density of " * eta)
  )

ggplot(data.frame(x = meuse.fit$delta)) +
  geom_density(aes(x = x)) +
  labs(
    x = expression(delta), y = "posterior density",
    title = expression("Posterior Density of " * delta)
  )

## ---- fig.height=3, fig.width=3.475, fig.show='hold'--------------------------
eta_acf <- acf(meuse.fit$eta, plot = FALSE)
eta_acfdf <- with(eta_acf, data.frame(lag, acf))

ggplot(data = eta_acfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  labs(
    x = "Lag", y = "ACF",
    title = expression("ACF for Samples from Posterior of " * eta)
  ) +
  theme(plot.title = element_text(hjust = 0.5))

delta_acf <- acf(meuse.fit$delta, plot = FALSE)
delta_acfdf <- with(delta_acf, data.frame(lag, acf))

ggplot(data = delta_acfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  labs(
    x = "Lag", y = "ACF",
    title = expression("ACF for Samples from Posterior of " * delta)
  ) +
  theme(plot.title = element_text(hjust = 0.5))

## ----fig.height=3, fig.width=3.475, fig.show='hold'---------------------------
eta_cummean_df <- data.frame(
  x = 1:length(meuse.fit$eta),
  y = cumsum(meuse.fit$eta) / (1:length(meuse.fit$eta))
)

ggplot(eta_cummean_df, aes(x = x, y = y)) +
  geom_line() +
  labs(
    x = "sample", y = expression(eta),
    title = bquote(atop("Cumulative Mean of Samples", "from Posterior of" ~ eta))
  ) +
  theme(plot.title = element_text(hjust = 0.5))

delta_cummean_df <- data.frame(
  x = 1:length(meuse.fit$delta),
  y = cumsum(meuse.fit$delta) / (1:length(meuse.fit$delta))
)

ggplot(delta_cummean_df, aes(x = x, y = y)) +
  geom_line() +
  labs(
    x = "sample", y = expression(delta),
    title = bquote(atop("Cumulative Mean of Samples", "from Posterior of" ~ delta))
  ) +
  theme(plot.title = element_text(hjust = 0.5))

## ---- fig.height=3, fig.width=3.475, fig.show='hold'--------------------------
Ypred_mat <- exp(predict(meuse.fit, meuse.valid))
meuse.valid$Ypred <- rowMeans(Ypred_mat)
min_value <- min(c(meuse.valid$Ypred, meuse.valid$zinc))
max_value <- max(c(meuse.valid$Ypred, meuse.valid$zinc))

ggplot(as.data.frame(meuse.valid), aes(x = Ypred, y = zinc)) +
  geom_point(size = 3) +
  geom_abline() +
  labs(
    x = "Predicted Values", y = "True Values",
    title = "Predicted vs. True Values"
  ) +
  xlim(min_value, max_value) +
  ylim(min_value, max_value) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(stack(as.data.frame(t(Ypred_mat)))) +
  geom_boxplot(aes(x = ind, y = values)) +
  geom_point(
    data = data.frame(Y.true = meuse.valid$zinc),
    aes(x = 1:9, y = Y.true),
    shape = 4, size = 3
  ) +
  labs(
    x = "", y = "Y",
    title = bquote(atop("Boxplot of Predicted Values of", "Y and True Values (X)"))
  ) +
  theme(plot.title = element_text(hjust = 0.5))

## -----------------------------------------------------------------------------
meuse.fit.covariates <- DSSP(
  formula = log(zinc) ~ log(lead) + lime, data = meuse.train, N = 10000,
  pars = c(0.001, 0.001), log_prior = function(x) -2 * log(1 + x)
)

summary(meuse.fit.covariates)

## ---- fig.height=7, fig.width=7-----------------------------------------------
plot(meuse.fit.covariates)

## ---- fig.height=3, fig.width=3.475, fig.show='hold'--------------------------
ggplot(
  data.frame(log_lead = meuse.fit.covariates$covariates_posterior["log(lead)", ]),
  aes(x = log_lead)
) +
  geom_density() +
  labs(
    title = "Posterior density of log(lead)",
    y = "posterior density", x = "log(lead)"
  )

ggplot(
  data.frame(lime = meuse.fit.covariates$covariates_posterior["lime", ]),
  aes(x = lime)
) +
  geom_density() +
  labs(
    title = "Posterior density of lime",
    y = "posterior density"
  )

