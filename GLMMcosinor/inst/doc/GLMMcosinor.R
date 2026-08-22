## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----srr, eval = FALSE, echo = FALSE------------------------------------------
#  #' @srrstats {G1.0}
#  #' @srrstats {G1.1}
#  #' @srrstats {G1.3}

## ----message=F, warning=F-----------------------------------------------------
library(GLMMcosinor)

cosinor_model <- cglmm(
  vit_d ~ X + amp_acro(time, period = 12, group = "X"),
  data = vitamind
)

## ----message=F, warning=F-----------------------------------------------------
head(cosinor_model$newdata)

## ----warning=F, message=F-----------------------------------------------------
cglmm(
  formula = vit_d ~ amp_acro(time, period = 12),
  data = vitamind,
  family = gaussian
)

## ----message=F, warning=F-----------------------------------------------------
cglmm(
  vit_d ~ X + amp_acro(time, period = 12, group = "X"),
  data = vitamind
)

## ----message=F, warning=F-----------------------------------------------------
cglmm(
  vit_d ~ 0 + X + amp_acro(time,
    period = 12,
    group = "X"
  ),
  data = vitamind
)

## -----------------------------------------------------------------------------
cosinor_model <- cglmm(
  vit_d ~ 0 + X + amp_acro(time,
    period = 12,
    group = "X"
  ),
  data = vitamind
)
autoplot(cosinor_model, predict.ribbon = FALSE)

## ----echo=F-------------------------------------------------------------------
withr::with_seed(
  50,
  {
    testdata_simple <- simulate_cosinor(
      1000,
      n_period = 2,
      mesor = 5,
      amp = 2,
      acro = 1,
      beta.mesor = 4,
      beta.amp = 1,
      beta.acro = 0.5,
      family = "poisson",
      period = 12,
      n_components = 1,
      beta.group = TRUE
    )
  }
)

## ----eval=F-------------------------------------------------------------------
#  testdata_simple <- simulate_cosinor(
#    1000,
#    n_period = 2,
#    mesor = 5,
#    amp = 2,
#    acro = 1,
#    beta.mesor = 4,
#    beta.amp = 1,
#    beta.acro = 0.5,
#    family = "poisson",
#    period = 12,
#    n_components = 1,
#    beta.group = TRUE
#  )

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ group + amp_acro(times, period = 12, group = "group"),
  data = testdata_simple, family = poisson()
)
summary(object)

## -----------------------------------------------------------------------------
test_cosinor_levels(object, x_str = "group", param = "amp")

## ----echo=F-------------------------------------------------------------------
withr::with_seed(
  50,
  {
    testdata_poisson <- simulate_cosinor(100,
      n_period = 2,
      mesor = 7,
      amp = c(0.1, 0.5),
      acro = c(1, 1),
      beta.mesor = 4.4,
      beta.amp = c(0.1, 0.46),
      beta.acro = c(0.5, -1.5),
      family = "poisson",
      period = c(12, 6),
      n_components = 2,
      beta.group = TRUE
    )
  }
)

## ----eval=F-------------------------------------------------------------------
#  testdata_poisson <- simulate_cosinor(100,
#    n_period = 2,
#    mesor = 7,
#    amp = c(0.1, 0.5),
#    acro = c(1, 1),
#    beta.mesor = 4.4,
#    beta.amp = c(0.1, 0.46),
#    beta.acro = c(0.5, -1.5),
#    family = "poisson",
#    period = c(12, 6),
#    n_components = 2,
#    beta.group = TRUE
#  )

## -----------------------------------------------------------------------------
cosinor_model <- cglmm(
  Y ~ group + amp_acro(times,
    period = c(12, 6),
    n_components = 2,
    group = "group"
  ),
  data = testdata_poisson,
  family = poisson()
)
test_cosinor_levels(
  cosinor_model,
  x_str = "group",
  param = "amp",
  component_index = 1
)

## -----------------------------------------------------------------------------
test_cosinor_components(
  cosinor_model,
  x_str = "group",
  param = "acr",
  level_index = 1
)

## ----eval=F-------------------------------------------------------------------
#  cbind(predictions = predict(cosinor_model, type = "response"), testdata_poisson)

## ----echo=F-------------------------------------------------------------------
head(cbind(
  predictions = predict(cosinor_model, type = "response"),
  testdata_poisson
))

## ----message=F, warning=F-----------------------------------------------------
autoplot(cosinor_model, superimpose.data = TRUE)

