## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----srr, eval = FALSE, echo = FALSE------------------------------------------
#  #' @srrstats {G2.0a}
#  #' @srrstats {G2.1a}
#  #' @srrstats {RE1.1}

## ----setup, message=FALSE-----------------------------------------------------
library(GLMMcosinor)
library(dplyr)

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
      period = c(12),
      n_components = 1,
      beta.group = TRUE
    )

    testdata_simple_gaussian <- simulate_cosinor(
      1000,
      n_period = 2,
      mesor = 5,
      amp = 2,
      acro = 1,
      beta.mesor = 4,
      beta.amp = 1,
      beta.acro = 0.5,
      family = "gaussian",
      period = c(12),
      n_components = 1,
      beta.group = TRUE
    )

    testdata_two_components <- simulate_cosinor(
      1000,
      n_period = 10,
      mesor = 7,
      amp = c(0.1, 0.4),
      acro = c(1, 1.5),
      beta.mesor = 4.4,
      beta.amp = c(2, 1),
      beta.acro = c(1, -1.5),
      family = "poisson",
      period = c(12, 6),
      n_components = 2,
      beta.group = TRUE
    )
  }
)

## ----eval = F-----------------------------------------------------------------
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
#    period = c(12),
#    n_components = 1,
#    beta.group = TRUE
#  )

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ amp_acro(times,
    period = 12
  ),
  data = filter(testdata_simple, group == 0),
  family = poisson()
)
object

## -----------------------------------------------------------------------------
autoplot(object, superimpose.data = TRUE)

## ----eval=F-------------------------------------------------------------------
#  testdata_simple_gaussian <- simulate_cosinor(
#    1000,
#    n_period = 2,
#    mesor = 5,
#    amp = 2,
#    acro = 1,
#    beta.mesor = 4,
#    beta.amp = 1,
#    beta.acro = 0.5,
#    family = "gaussian",
#    period = c(12),
#    n_components = 1,
#    beta.group = TRUE
#  )

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ amp_acro(times,
    period = 12,
    group = "group"
  ),
  data = testdata_simple_gaussian,
  family = gaussian
)
object

## -----------------------------------------------------------------------------
autoplot(object)

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ group + amp_acro(times,
    period = 12,
    group = "group"
  ),
  data = testdata_simple_gaussian,
  family = gaussian()
)
object

## -----------------------------------------------------------------------------
autoplot(object)

## ----message=F, warning=F-----------------------------------------------------
cglmm(
  Y ~ 0 + group + amp_acro(times,
    period = 12,
    group = "group"
  ),
  data = testdata_simple,
  family = poisson()
)

## ----eval=F-------------------------------------------------------------------
#  testdata_two_components <- simulate_cosinor(
#    1000,
#    n_period = 10,
#    mesor = 7,
#    amp = c(0.1, 0.4),
#    acro = c(1, 1.5),
#    beta.mesor = 4.4,
#    beta.amp = c(2, 1),
#    beta.acro = c(1, -1.5),
#    family = "poisson",
#    period = c(12, 6),
#    n_components = 2,
#    beta.group = TRUE
#  )

## ----message=F, warning=F-----------------------------------------------------
cglmm(
  Y ~ group + amp_acro(
    time_col = times,
    n_components = 2,
    period = c(12, 6),
    group = c("group", "group")
  ),
  data = testdata_two_components,
  family = poisson()
)

## ----message=F, warning=F-----------------------------------------------------
testdata_disp_zi <- simulate_cosinor(1000,
  n_period = 6,
  mesor = 7,
  amp = c(0.1, 0.4, 0.5),
  acro = c(1, 1.5, 0.1),
  beta.mesor = 4.4,
  beta.amp = c(2, 1, 0.4),
  beta.acro = c(1, -1.5, -1),
  family = "gaussian",
  period = c(12, 6, 8),
  n_components = 3
)
object_disp_zi <- cglmm(
  Y ~ group + amp_acro(times,
    n_components = 3,
    period = c(12, 6, 8),
    group = "group"
  ),
  data = testdata_disp_zi, family = gaussian(),
  dispformula = ~ group + amp_acro(times,
    n_components = 2,
    group = "group",
    period = c(12, 6)
  ),
  ziformula = ~ group + amp_acro(times,
    n_components = 3,
    group = "group",
    period = c(7, 8, 2)
  )
)

object_disp_zi

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ group + amp_acro(times,
    period = 12,
    group = "group"
  ),
  data = testdata_simple,
  family = poisson()
)
summary(object)

## ----message=F, warning=F-----------------------------------------------------
summary(object_disp_zi)

## -----------------------------------------------------------------------------
library(DHARMa)
plotResiduals(simulateResiduals(object$fit))
plotQQunif(simulateResiduals(object$fit))

