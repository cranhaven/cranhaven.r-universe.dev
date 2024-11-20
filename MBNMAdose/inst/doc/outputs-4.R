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

## -----------------------------------------------------------------------------
# Specify treatments (agents and doses) for which to estimate relative effects
treats <- list("Placebo"=0,
               "eletriptan"= 1,
               "sumatriptan"=2,
               "almotriptan"=1)

# Print relative effects on the natural scale
rels <- get.relative(trip.emax, treatments = treats, eform=TRUE)
print(rels)

# Rank relative effects
rank(rels)

## ----results="hide"-----------------------------------------------------------
nma <- nma.run(tripnet) # NMA (consistency) model
ume <- nma.run(tripnet, UME=TRUE) # UME (inconsistency) model

## -----------------------------------------------------------------------------
# MBNMA consistency and NMA consistency odds ratios compared 
consistency <- get.relative(lower.diag=trip.emax, upper.diag=nma,
                            treatments = treats, eform=TRUE)
print(consistency)

## -----------------------------------------------------------------------------
# MBNMA consistency and NMA inconsistency log-odds ratios compared
inconsistency <- get.relative(lower.diag=trip.emax, upper.diag=ume,
                            treatments = treats, eform=FALSE)
print(inconsistency)

## ----results="hide"-----------------------------------------------------------
plot(trip.emax)

## -----------------------------------------------------------------------------
ranks <- rank(trip.emax, lower_better = FALSE)
print(ranks)
summary(ranks)

## -----------------------------------------------------------------------------
# Ranking histograms for Emax
plot(ranks, params = "emax")

# Ranking histograms for ED50
plot(ranks, params = "ed50")

## -----------------------------------------------------------------------------
# Cumulative ranking plot for both dose-response parameters
cumrank(ranks, sucra=TRUE)

