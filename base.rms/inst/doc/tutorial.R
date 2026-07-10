## ----warning=FALSE,message=FALSE----------------------------------------------
library(rms)
library(base.rms)
library(survival)

## ----warning=FALSE------------------------------------------------------------
fit <- lm(mpg~cyl+vs,data=mtcars)
lm2ols(fit)

## -----------------------------------------------------------------------------
fit <- ols(mpg~cyl+vs,data=mtcars)
ols2lm(fit)

## -----------------------------------------------------------------------------
fit <- glm(vs~mpg,data=mtcars,family = binomial(link='logit'))
logit2lrm(fit)

## -----------------------------------------------------------------------------
fit <- lrm(vs~mpg,data=mtcars)
lrm2logit(fit)

## -----------------------------------------------------------------------------
fit <- coxph(Surv(mpg,vs)~am+gear,data=mtcars)
coxph2cph(fit)

## -----------------------------------------------------------------------------
fit <- cph(Surv(mpg,vs)~am+gear,data=mtcars)
cph2coxph(fit)

