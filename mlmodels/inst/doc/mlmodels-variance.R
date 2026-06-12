## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mlmodels)

## ----oim----------------------------------------------------------------------
data(mroz)
mroz$incthou <- mroz$faminc / 1000

# Fit a homoskedastic linear model
fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, 
             data = mroz)

# Summary calls vcov() which uses oim as default.
summary(fit)

v_oim <- vcov(fit, type = "oim")
sqrt(diag(v_oim))

## ----opg----------------------------------------------------------------------
v_opg <- vcov(fit, type = "opg")

## ----opg-oim-comp-------------------------------------------------------------
comp <- data.frame(
  oim = sqrt(diag(v_oim)),
  opg = sqrt(diag(v_opg))
)

comp

## ----logt-oim-----------------------------------------------------------------

fit_het <- ml_logit(inlf ~ age + I(age^2) + huswage + educ + unem,
                    scale = ~ repwage,
                    data = mroz)

summary(fit_het, vcov.type = "oim")

## ----robust-------------------------------------------------------------------
  # summary() with the vcov.type argument
summary(fit, vcov.type = "robust")

# or vcov() with the type argument
v_robust <- vcov(fit, type = "robust")

## ----robust-pre---------------------------------------------------------------
summary(fit, vcov = v_robust)

## ----robust-cluster-----------------------------------------------------------
# Suspect that income is correlated among education levels.
v_rob_clust <- vcov(fit, type = "robust", cl_var = "educ")
summary(fit, vcov = v_rob_clust)

## ----boot-jack----------------------------------------------------------------
# Bootstrap with 20 repetitions
v_boot <- vcov(fit, type = "boot", repetitions = 20)
# Jackknife
v_jack <- vcov(fit, type = "jack")

## ----boot-jack-clust----------------------------------------------------------
# Bootstrap with 20 repetitions
v_boot_clust <- vcov(fit, type = "boot", cl_var = "educ",
               repetitions = 20,
               progress = FALSE)
# Jackknife
v_jack_clust <- vcov(fit, type = "jack", cl_var = "educ",
               progress = FALSE)

## ----boot-jack-summary--------------------------------------------------------
# Robust Comparison
comp<- data.frame(
  robust = sqrt(diag(v_robust)),
  boot = sqrt(diag(v_boot)),
  jack = sqrt(diag(v_jack))
)
comp

# Clustered comparison
comp <- data.frame(
  robust = sqrt(diag(v_rob_clust)),
  boot = sqrt(diag(v_boot_clust)),
  jack = sqrt(diag(v_jack_clust))
)
comp

