## ----setup, include = FALSE---------------------------------------------------
library(MBNMAdose)
#devtools::load_all()
library(rmarkdown)
library(knitr)
library(dplyr)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  include=TRUE,
  tidy.opts=list(width.cutoff=80),
  tidy=TRUE
)

## ----reg.prep, results="hide"-------------------------------------------------
# Using the SSRI dataset
ssri.reg <- ssri

# For a continuous covariate
ssri.reg <- ssri.reg %>% 
  dplyr::mutate(x.weeks = weeks - mean(weeks, na.rm=TRUE))

# For a categorical covariate
table(ssri$weeks) # Using 8 weeks as the reference
ssri.reg <- ssri.reg %>% 
  dplyr::mutate(r.weeks=factor(weeks, levels=c(8,4,5,6,9,10)))

# Create network object
ssrinet <- mbnma.network(ssri.reg)

## ----results="hide", message=FALSE--------------------------------------------
# Regress for continuous weeks
# Separate effect modification for each agent vs Placebo
ssrimod.a <- mbnma.run(ssrinet, fun=dfpoly(degree=2), 
                     regress=~x.weeks, regress.effect = "agent")

## -----------------------------------------------------------------------------
summary(ssrimod.a)

## ----results="hide", message=FALSE--------------------------------------------
# Regress for continuous weeks
# Random effect modification across all agents vs Placebo
ssrimod.r <- mbnma.run(ssrinet, fun=dfpoly(degree=2), 
                     regress=~x.weeks, regress.effect = "random")

## -----------------------------------------------------------------------------
summary(ssrimod.r)

## ----results="hide", message=FALSE--------------------------------------------
# Regress for categorical weeks
# Common effect modification across all agents vs Placebo
ssrimod.c <- mbnma.run(ssrinet, fun=dfpoly(degree=2), 
                     regress=~r.weeks, regress.effect = "common")

## -----------------------------------------------------------------------------
summary(ssrimod.c)

## -----------------------------------------------------------------------------
# For a continuous covariate, make predictions at 5 weeks follow-up
pred <- predict(ssrimod.a, regress.vals=c("x.weeks"=5))
plot(pred)

## -----------------------------------------------------------------------------
# For a categorical covariate, make predictions at 10 weeks follow-up
regress.p <- c("r.weeks10"=1, "r.weeks4"=0, "r.weeks5"=0, 
               "r.weeks6"=0, "r.weeks9"=0)

pred <- predict(ssrimod.c, regress.vals=regress.p)
plot(pred)

