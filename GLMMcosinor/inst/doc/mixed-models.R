## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(GLMMcosinor)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
f_sample_id <- function(id_num,
                        n = 30,
                        mesor,
                        amp,
                        acro,
                        family = "gaussian",
                        sd = 0.2,
                        period,
                        n_components,
                        beta.group = TRUE) {
  data <- simulate_cosinor(
    n = n,
    mesor = mesor,
    amp = amp,
    acro = acro,
    family = family,
    sd = sd,
    period = period,
    n_components = n_components
  )
  data$subject <- id_num
  data
}

dat_mixed <- do.call(
  "rbind",
  lapply(1:30, function(x) {
    f_sample_id(
      id_num = x,
      mesor = rnorm(1, mean = 0, sd = 1),
      amp = rnorm(1, mean = 3, sd = 0.5),
      acro = rnorm(1, mean = 1.5, sd = 0.2),
      period = 24,
      n_components = 1
    )
  })
)
dat_mixed$subject <- as.factor(dat_mixed$subject)

## ----echo=FALSE---------------------------------------------------------------
withr::with_seed(
  50,
  {
    dat_mixed <- do.call(
      "rbind",
      lapply(1:30, function(x) {
        f_sample_id(
          id_num = x,
          mesor = rnorm(1, mean = 0, sd = 1),
          amp = rnorm(1, mean = 3, sd = 0.5),
          acro = rnorm(1, mean = 1.5, sd = 0.2),
          period = 24,
          n_components = 1
        )
      })
    )
    dat_mixed$subject <- as.factor(dat_mixed$subject)
  }
)

## -----------------------------------------------------------------------------
ggplot(dat_mixed, aes(times, Y, col = subject)) +
  geom_point() +
  geom_line() +
  theme_bw()

## ----echo=FALSE---------------------------------------------------------------
withr::with_seed(42, {
  mixed_mod <- cglmm(
    Y ~ amp_acro(times, n_components = 1, period = 24) +
      (1 + amp_acro1 | subject),
    data = dat_mixed
  )
})

## -----------------------------------------------------------------------------
mixed_mod$formula

## -----------------------------------------------------------------------------
autoplot(mixed_mod, superimpose.data = TRUE)

## -----------------------------------------------------------------------------
summary(mixed_mod)

## -----------------------------------------------------------------------------
ggplot(cbind(dat_mixed, pred = predict(mixed_mod))) +
  geom_point(aes(x = times, y = Y, col = subject)) +
  geom_line(aes(x = times, y = pred, col = subject))

## -----------------------------------------------------------------------------
fixed_effects_mod <- cglmm(
  Y ~ amp_acro(times, n_components = 1, period = 24),
  data = dat_mixed
)

AIC(fixed_effects_mod$fit)
AIC(mixed_mod$fit)

