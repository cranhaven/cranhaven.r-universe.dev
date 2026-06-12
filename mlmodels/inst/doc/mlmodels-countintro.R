## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mlmodels)

## ----load-data----------------------------------------------------------------
  data("docvis", package = "mlmodels")

## -----------------------------------------------------------------------------
  mean_for <- docvis ~ private + medicaid + age + I(age^2) + educyr + actlim + totchr
  pois <- ml_poisson(mean_for, data = docvis)

## -----------------------------------------------------------------------------
  # oim standard errors are the default in summary
  summary(pois)

## ----poisson-robust-----------------------------------------------------------
summary(pois, vcov.type = "robust")

## ----overdispersion-test------------------------------------------------------
  OVDtest(pois)

## ----ames---------------------------------------------------------------------
  library(marginaleffects)
  avg_slopes(pois, vcov = "robust")

## ----nb-estimation------------------------------------------------------------
# NB1 (linear variance)
nb1 <- ml_negbin(mean_for, data = docvis, dispersion = "NB1")
summary(nb1, vcov.type = "robust")

# NB2 (quadratic variance — default)
nb2 <- ml_negbin(mean_for, data = docvis)
summary(nb2, vcov.type = "robust")

## ----mean-comp----------------------------------------------------------------
mpoi <- predict(pois, vcov.type = "robust", se.fit = TRUE)
mnb1 <- predict(nb1, vcov.type = "robust", se.fit = TRUE)
mnb2 <- predict(nb2, vcov.type = "robust", se.fit = TRUE)

comp <- data.frame(
  poi_mean = mpoi$fit,
  poi_se   = mpoi$se.fit,
  nb1_mean = mnb1$fit,
  nb1_se   = mnb1$se.fit,
  nb2_mean = mnb2$fit,
  nb2_se   = mnb2$se.fit
)

comp[1:10, ]

## ----avg-mean-----------------------------------------------------------------
avg_predictions(pois, vcov = "robust")
avg_predictions(nb1, vcov = "robust")
avg_predictions(nb2, vcov = "robust")

## ----avg-slopes---------------------------------------------------------------
avg_slopes(pois, vcov = "robust")
avg_slopes(nb1, vcov = "robust")
avg_slopes(nb2, vcov = "robust")

## ----nb-vuong-----------------------------------------------------------------
vuongtest(nb1, nb2)

## ----gof----------------------------------------------------------------------
# Default bins are 0 to 5
GOFtest(pois)
GOFtest(nb1)
GOFtest(nb2)

## ----nb-hetero----------------------------------------------------------------
nb1_het <- ml_negbin(mean_for, 
                     scale = ~ female + bh, 
                     dispersion = "NB1", 
                     data = docvis)

nb2_het <- ml_negbin(mean_for, 
                     scale = ~ female + bh, 
                     data = docvis)

summary(nb1_het, vcov.type = "robust")
summary(nb2_het, vcov.type = "robust")

## ----nb-het-test--------------------------------------------------------------
vuongtest(nb1_het, nb2_het)
GOFtest(nb1_het)
GOFtest(nb2_het)

