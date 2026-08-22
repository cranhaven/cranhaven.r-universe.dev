## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=F-------------------------------------------------------------------
library(GLMMcosinor)
withr::with_seed(
  50,
  {
    testdata_two_components <- simulate_cosinor(1000,
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

    testdata_two_components_grouped <- simulate_cosinor(1000,
      n_period = 5,
      mesor = 3.7,
      amp = c(0.1, 0.4),
      acro = c(1, 1.5),
      beta.mesor = 4,
      beta.amp = c(2, 0.4),
      beta.acro = c(1, 1.5),
      family = "poisson",
      period = c(12, 6),
      n_components = 2,
      beta.group = TRUE
    )

    testdata_three_components <- simulate_cosinor(1000,
      n_period = 2,
      mesor = 7,
      amp = c(0.1, 0.4, 0.5),
      acro = c(1, 1.5, 0.1),
      beta.mesor = 4.4,
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, -1.5, -1),
      family = "poisson",
      period = c(12, 6, 12),
      n_components = 3,
      beta.group = TRUE
    )
  }
)

## ----eval=F-------------------------------------------------------------------
#  library(GLMMcosinor)
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
object <- cglmm(
  Y ~ group + amp_acro(
    time_col = times,
    n_components = 2,
    period = c(12, 6),
    group = c("group", "group")
  ),
  data = testdata_two_components,
  family = poisson()
)
object

## -----------------------------------------------------------------------------
autoplot(object)

## ----eval=F-------------------------------------------------------------------
#  testdata_two_components_grouped <- simulate_cosinor(
#    1000,
#    n_period = 5,
#    mesor = 3.7,
#    amp = c(0.1, 0.4),
#    acro = c(1, 1.5),
#    beta.mesor = 4,
#    beta.amp = c(2, 0.4),
#    beta.acro = c(1, 1.5),
#    family = "poisson",
#    period = c(12, 6),
#    n_components = 2,
#    beta.group = TRUE
#  )

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ group + amp_acro(
    time_col = times,
    n_components = 2,
    period = c(12, 6),
    group = c("group", NA)
  ),
  data = testdata_two_components_grouped,
  family = poisson()
)
object

## -----------------------------------------------------------------------------
autoplot(object, superimpose.data = TRUE)

## -----------------------------------------------------------------------------
object <- cglmm(
  Y ~ group + amp_acro(
    time_col = times,
    n_components = 2,
    period = c(12, 6),
    group = c("group", "group")
  ),
  data = testdata_two_components_grouped,
  family = poisson()
)
object

## ----message=F, warning=F-----------------------------------------------------
cglmm(
  Y ~ group + amp_acro(times,
    n_components = 2,
    period = 12,
    group = "group"
  ),
  data = testdata_two_components,
  family = poisson()
)


cglmm(
  Y ~ group + amp_acro(times,
    n_components = 2,
    period = c(12, 12),
    group = c("group", "group")
  ),
  data = testdata_two_components,
  family = poisson()
)

## ----eval=F-------------------------------------------------------------------
#  testdata_three_components <- simulate_cosinor(
#    1000,
#    n_period = 2,
#    mesor = 7,
#    amp = c(0.1, 0.4, 0.5),
#    acro = c(1, 1.5, 0.1),
#    beta.mesor = 4.4,
#    beta.amp = c(2, 1, 0.4),
#    beta.acro = c(1, -1.5, -1),
#    family = "poisson",
#    period = c(12, 6, 12),
#    n_components = 3,
#    beta.group = TRUE
#  )

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ group + amp_acro(times,
    n_components = 3,
    period = c(12, 6, 12),
    group = "group"
  ),
  data = testdata_three_components,
  family = poisson()
)
autoplot(object,
  superimpose.data = TRUE,
  x_str = "group",
  predict.ribbon = FALSE,
  data_opacity = 0.08
)

