## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=F, message=FALSE----------------------------------------------------
withr::with_seed(42, {
  testdata_two_components <- GLMMcosinor::simulate_cosinor(
    1000,
    n_period = 2,
    mesor = 1,
    amp = c(0.1, 0.4),
    acro = c(1, 1.5),
    beta.mesor = 1.1,
    beta.amp = c(0.4, 0.1),
    beta.acro = c(1, 1.2),
    family = "poisson",
    period = c(12, 6),
    n_components = 2
  )

  testdata_period_diff <- GLMMcosinor::simulate_cosinor(
    1000,
    n_period = 1,
    mesor = 7,
    amp = c(0.1, 0.4),
    acro = c(1, 1.5),
    family = "poisson",
    period = c(12, 1000),
    n_components = 2
  )
})

## ----message=F, warning=F-----------------------------------------------------
library(GLMMcosinor)

object <- cglmm(
  vit_d ~ X + amp_acro(time,
    group = "X",
    period = 12
  ),
  data = vitamind
)
autoplot(object, x_str = "X")

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  vit_d ~ X + amp_acro(time,
    group = "X",
    period = 12
  ),
  data = vitamind
)
autoplot(object, x_str = "X", superimpose.data = TRUE)

## ----echo=F, message=F--------------------------------------------------------
testdata_two_components <- simulate_cosinor(
  1000,
  n_period = 2,
  mesor = 1,
  amp = c(0.1, 0.4),
  acro = c(1, 1.5),
  beta.mesor = 1.1,
  beta.amp = c(0.4, 0.1),
  beta.acro = c(1, 1.2),
  family = "poisson",
  period = c(12, 6),
  n_components = 2
)

## ----message=F, warning=F-----------------------------------------------------
testdata_two_components <- testdata_two_components
testdata_two_components$X <- rbinom(length(testdata_two_components$group),
  2,
  prob = 0.5
)
object <- cglmm(
  Y ~ group + amp_acro(times,
    n_components = 2,
    period = c(12, 6),
    group = c("group", "X")
  ),
  data = testdata_two_components,
  family = poisson()
)
autoplot(object, predict.ribbon = FALSE)

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ group + amp_acro(times,
    n_components = 2,
    period = c(12, 6),
    group = c("group", "X")
  ),
  data = testdata_two_components,
  family = poisson()
)
autoplot(object, x_str = "X", predict.ribbon = FALSE)

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ group + amp_acro(times,
    n_components = 2,
    period = c(12, 6),
    group = c("group", "X")
  ),
  data = testdata_two_components,
  family = poisson()
)
autoplot(object, x_str = "group", predict.ribbon = FALSE)

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ group + amp_acro(times,
    n_components = 2,
    period = c(12, 6),
    group = c("group", "X")
  ),
  data = testdata_two_components,
  family = poisson()
)
autoplot(object, x_str = "group", predict.ribbon = TRUE, xlims = c(13, 15))

## ----eval=F-------------------------------------------------------------------
#  testdata_period_diff <- simulate_cosinor(
#    1000,
#    n_period = 1,
#    mesor = 7,
#    amp = c(0.1, 0.4),
#    acro = c(1, 1.5),
#    family = "poisson",
#    period = c(12, 1000),
#    n_components = 2
#  )

## ----message=F, warning=F-----------------------------------------------------
object <- cglmm(
  Y ~ amp_acro(times,
    n_components = 2,
    period = c(12, 1000)
  ),
  data = testdata_period_diff,
  family = poisson()
)

autoplot(object, points_per_min_cycle_length = 40)

## ----message=F, warning=F-----------------------------------------------------
model <- cglmm(
  vit_d ~ X + amp_acro(time,
    group = "X",
    period = 12
  ),
  data = vitamind
)
polar_plot(model)

## ----message=F, warning=F-----------------------------------------------------
model <- cglmm(
  vit_d ~ X + amp_acro(time,
    group = "X",
    period = 12
  ),
  data = vitamind
)
polar_plot(model, radial_units = "degrees")

## ----message=F, warning=F-----------------------------------------------------
model <- cglmm(
  vit_d ~ X + amp_acro(time,
    group = "X",
    period = 12
  ),
  data = vitamind
)
polar_plot(model, overlay_parameter_info = TRUE)

## ----message=F, warning=F-----------------------------------------------------
model <- cglmm(
  vit_d ~ X + amp_acro(time,
    group = "X",
    period = 12
  ),
  data = vitamind
)
polar_plot(model,
  grid_angle_segments = 12,
  clockwise = TRUE,
  start = "top",
  n_breaks = 5
)

## ----message=F, warning=F-----------------------------------------------------
model <- cglmm(
  vit_d ~ X + amp_acro(time,
    group = "X",
    period = 12
  ),
  data = vitamind
)
polar_plot(model,
  grid_angle_segments = 12,
  clockwise = TRUE,
  start = "top",
  view = "zoom_origin"
)

