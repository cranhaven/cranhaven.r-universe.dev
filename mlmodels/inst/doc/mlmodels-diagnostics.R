## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mlmodels)

## ----lin-imtest---------------------------------------------------------------
data(mroz)
mroz$incthou <- mroz$faminc / 1000

# Fit a homoskedastic linear model
fit_lm <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, 
             data = mroz)

# Default is quad (quadratic orthognalized)
IMtest(fit_lm)

# Let's check the Chesher/Lancaster
IMtest(fit_lm, method = "opg")

## ----lin-imtest-boot----------------------------------------------------------
# Bootstrapped quadratic (low repetitions for speed)
IMtest(fit_lm, method = "boot_quad", repetitions = 20, seed = 123)

# Boot opg (low repetitions for speed)
IMtest(fit_lm, method = "boot_opg", repetitions = 20, seed = 123)

## ----gamma-imtest-------------------------------------------------------------
fit_gam <- ml_gamma(incthou ~ age + I(age^2) + huswage + educ + unem, 
             data = mroz)

IMtest(fit_gam, method = "boot_quad", repetitions = 20, seed = 123)

IMtest(fit_gam, method = "boot_opg", repetitions = 20, seed = 123)

## ----gamma-results------------------------------------------------------------
# Show results with oim standard errors
summary(fit_gam)

summary(fit_gam, vcov.type = "robust")

## ----gamma-lrtest-------------------------------------------------------------
fit_gam_ur <- ml_gamma(incthou ~ age + I(age^2) + huswage + educ + unem + 
                         kidslt6 + kidsge6, 
                       data = mroz)

summary(fit_gam_ur)

# The order you enter the models doesn't matter
lrtest(fit_gam_ur, fit_gam)

## ----waldtest-one-------------------------------------------------------------
# Using indices
waldtest(fit_gam_ur, indices = 7:8)

# Using coef_names
waldtest(fit_gam_ur, coef_names = c("value::kidslt6", "value::kidsge6"))

## ----waldtest-two-------------------------------------------------------------
# Testing that age equals .05 and I(age^2) equals 0
waldtest(fit_gam_ur, coef_names = c("value::age", "value::I(age^2)"), rhs = c(.05, 0))

# Testing age and educ both equal .05
waldtest(fit_gam_ur, coef_names = c("value::age", "value::educ"), rhs = .05)

## ----waldtest-three-----------------------------------------------------------
# One restriction, one row in the matrix with the vector.
R <- rbind(c(0, 1, 0, 0, -1, 0, 0, 0, 0))

# Do the test (rhs is 0, which is the default)
waldtest(fit_gam_ur, rest_matrix = R)

# Test both that they're both equal and that age equals .05
R <- rbind(R,                             # age - value (from before)
           c(0, 1, 0, 0, 0, 0, 0, 0, 0))  # only age

# Don't forget the rhs are different
waldtest(fit_gam_ur, rest_matrix = R, rhs = c(0, .05))

## ----waldtest-four------------------------------------------------------------
# Get the indices of the coefficients for the value equation and drop the intercept.
idx <- grep("value::", names(coef(fit_gam_ur)))[-1]

waldtest(fit_gam_ur, indices = idx)

## ----waldtest-five------------------------------------------------------------
# robust (sandwich) variance
waldtest(fit_gam_ur, indices = idx, vcov.type = "robust")

## ----vuong--------------------------------------------------------------------
fit_ln <- ml_lm(log(incthou) ~ age + I(age^2) + huswage + educ + unem, 
             data = mroz)

IMtest(fit_ln, method = "boot_quad", repetitions = 20, seed = 123)

vuongtest(fit_gam, fit_ln)

## ----poisson------------------------------------------------------------------
data("docvis")
fit_poi <- ml_poisson(docvis ~ private + medicaid + age + I(age^2) + educyr + actlim + totchr,
                      data = docvis)

OVDtest(fit_poi)

## ----possin-gof---------------------------------------------------------------
# Default bins are individual values from 0 to 5.
GOFtest(fit_poi)

