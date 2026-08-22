## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GLMMcosinor)

## ----message=F, warning=F, eval=F---------------------------------------------
#  testdata <- simulate_cosinor(
#    n = 200,
#    mesor = 1,
#    amp = 2,
#    acro = 1.2,
#    period = 12,
#    n_period = 3,
#    family = "poisson"
#  )
#  
#  testdata

## ----message=F, warning=F, eval=F---------------------------------------------
#  object <- cglmm(
#    Y ~ amp_acro(times,
#      n_components = 1,
#      period = 12
#    ),
#    data = testdata,
#    family = poisson()
#  )
#  summary(object)
#  autoplot(object, superimpose.data = TRUE)

## ----message=F, warning=F, eval=F---------------------------------------------
#  simulate_cosinor(
#    n = 100,
#    mesor = 1,
#    amp = 2,
#    acro = 1.2,
#    period = 12,
#    beta.group = TRUE,
#    beta.mesor = 0.4,
#    beta.amp = 0.5,
#    beta.acro = 0.2,
#    n_period = 3,
#    n_components = 1,
#    family = "poisson"
#  )

## ----message=F, warning=F, echo=F---------------------------------------------
testdata <- simulate_cosinor(
  n = 100,
  mesor = 1,
  amp = 2,
  acro = 1.2,
  period = 12,
  beta.group = TRUE,
  beta.mesor = 0.4,
  beta.amp = 0.5,
  beta.acro = 0.2,
  n_components = 1,
  n_period = 3,
  family = "gaussian"
)

DT::datatable(testdata)
object <- cglmm(
  Y ~ group + amp_acro(times,
    n_components = 1,
    period = 12,
    group = "group"
  ),
  data = testdata,
  family = gaussian()
)
summary(object)
autoplot(object, superimpose.data = TRUE)

## ----message=F, warning=F, eval=F---------------------------------------------
#  testdata <- simulate_cosinor(
#    n = 200,
#    mesor = 1,
#    amp = c(0.2, 1),
#    acro = c(1.2, 2),
#    period = c(12, 6),
#    n_components = 2,
#    n_period = 2,
#    family = "poisson"
#  )
#  
#  testdata
#  object <- cglmm(
#    Y ~ amp_acro(times,
#      n_components = 2,
#      period = c(12, 6)
#    ),
#    data = testdata,
#    family = poisson()
#  )
#  summary(object)
#  autoplot(object, superimpose.data = TRUE)

## ----message=F, warning=F, fig.show = "hold"----------------------------------
testdata <- simulate_cosinor(100,
  mesor = 7,
  amp = c(0.1, 0.4, 0.5),
  acro = c(1, 1.5, 0.1),
  beta.mesor = 4.4,
  beta.amp = c(2, 1, 0.4),
  beta.acro = c(1, -1.5, -1),
  family = "poisson",
  period = c(12, 6, 8),
  n_period = 2,
  n_components = 3
)

object <- cglmm(Y ~ group + amp_acro(times,
  n_components = 3,
  period = c(12, 6, 8),
  group = "group"
), data = testdata, family = poisson())
summary(object)
autoplot(object,
  superimpose.data = TRUE,
  x_str = "group",
  predict.ribbon = FALSE
)

## ----message=F, warning=F, fig.show = "hold"----------------------------------
testdata <- simulate_cosinor(500,
  mesor = 1,
  amp = c(0.5, 0.5, 0.5),
  acro = c(pi, pi / 2, pi),
  alpha = 2,
  beta.mesor = 2,
  beta.amp = c(0.2, 0.2, 0.2),
  beta.acro = c(pi / 2, pi, pi / 2),
  beta.alpha = 3,
  family = "gamma",
  period = c(12, 6, 8),
  n_period = 2,
  n_components = 3
)

object <- cglmm(Y ~ group + amp_acro(times,
  n_components = 3,
  period = c(12, 6, 8),
  group = "group"
), data = testdata, family = Gamma(link = "log"))
summary(object)
autoplot(object,
  superimpose.data = TRUE,
  x_str = "group",
  predict.ribbon = FALSE
)

