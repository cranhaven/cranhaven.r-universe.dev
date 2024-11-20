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

## ----results="hide"-----------------------------------------------------------
tripnet <- mbnma.network(triptans)
trip.emax <- mbnma.run(tripnet, fun=demax(emax="rel", ed50="rel")) 

## ----eval=FALSE---------------------------------------------------------------
#  E0 <- triptans[triptans$dose==0,]

## -----------------------------------------------------------------------------
# Predict 20 doses for each agent, with a stochastic distribution for E0
doses <- list("Placebo"=0, 
                  "eletriptan"=3,
                  "sumatriptan"=3,
                  "almotriptan"=3,
                  "zolmitriptan"=3,
                  "naratriptan"=3,
                  "rizatriptan"=3)

pred <- predict(trip.emax, E0="rbeta(n, shape1=2, shape2=10)",
                      max.doses=doses, n.dose=20)


# Predict exact doses for two agents, and estimate E0 from the data
E0.data <- triptans[triptans$dose==0,]
doses <- list("eletriptan"=c(0,1,3),
                  "sumatriptan"=c(0,3))

## ----results="hide"-----------------------------------------------------------
pred <- predict(trip.emax, E0=E0.data,
                      exact.doses=doses)

## -----------------------------------------------------------------------------
summary(pred)

## -----------------------------------------------------------------------------
# Predict responses using default doses up to the maximum of each agent in the dataset
pred <- predict(trip.emax, E0=0.2, n.dose=20)

plot(pred)

## -----------------------------------------------------------------------------
plot(pred, disp.obs = TRUE)

## ----results="hide", warning=FALSE, message=FALSE-----------------------------
alognet <- mbnma.network(alog_pcfb)
alog.emax <- mbnma.run(alognet, fun=demax(), method="random")
pred <- predict(alog.emax, E0=0, n.dose=20)
plot(pred, overlay.split = TRUE, method="random")

## -----------------------------------------------------------------------------
pred <- predict(trip.emax, E0=0.2, n.doses=4,
                max.doses = list("eletriptan"=5, "sumatriptan"=5, 
                              "frovatriptan"=5, "zolmitriptan"=5))

ranks <- rank(pred)
plot(ranks)

