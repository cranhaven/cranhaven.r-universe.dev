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

## -----------------------------------------------------------------------------
# Using the triptans dataset
network <- mbnma.network(triptans)
summary(network)

## ----message=FALSE, warning=FALSE---------------------------------------------
# Prepare data using the gout dataset
goutnet <- mbnma.network(gout)
summary(goutnet)

## -----------------------------------------------------------------------------
plot(goutnet, label.distance = 5)

## -----------------------------------------------------------------------------
# Plot at the agent-level
plot(goutnet, level="agent", label.distance = 6)

## -----------------------------------------------------------------------------
# Plot connections to placebo via a two-parameter dose-response function (e.g. Emax)
plot(goutnet, level="agent", doselink = 2, remove.loops = TRUE, label.distance = 6)

## ----results="hide"-----------------------------------------------------------
# Colour vertices by agent
plot(goutnet, v.color = "agent", label.distance = 5)

## ----results="hide", message=FALSE, warning=FALSE-----------------------------
# Run a random effect split NMA using the alogliptin dataset
alognet <- mbnma.network(alog_pcfb)
nma.alog <- nma.run(alognet, method="random")

## -----------------------------------------------------------------------------
print(nma.alog)

# Draw plot of NMA estimates plotted by dose
plot(nma.alog)

