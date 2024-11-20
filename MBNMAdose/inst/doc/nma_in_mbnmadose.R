## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  include=TRUE,
  tidy.opts=list(width.cutoff=80),
  tidy=TRUE
)

## ----setup, include = FALSE---------------------------------------------------
library(MBNMAdose)
library(dplyr)

## ----loaddata-----------------------------------------------------------------
library(netmeta)
data("Dong2013")

# Rename column names to match those used in MBNMAdose
Dong2013 <- Dong2013 %>% 
  rename(studyID = id,
         r = death,
         n = randomized)

## -----------------------------------------------------------------------------
# Define agents and assign a dose of 1 to all agents
Dong2013 <- Dong2013 %>% 
  dplyr::rename(agent=treatment) %>%
  dplyr::mutate(dose=dplyr::case_when(agent=="Placebo" ~ 0,
                                      agent!="Placebo" ~ 1))

## ----network.plot, message=FALSE----------------------------------------------
network <- mbnma.network(Dong2013)

## -----------------------------------------------------------------------------
summary(network)
plot(network)

## ----standard.nma, results="hide"---------------------------------------------
nma.linear <- mbnma.run(network, fun=dpoly(degree=1),
                        n.iter=50000)

## -----------------------------------------------------------------------------
print(nma.linear)

## ----results="hide"-----------------------------------------------------------
nma <- nma.run(network, n.iter=50000)

## -----------------------------------------------------------------------------
print(nma)

## -----------------------------------------------------------------------------
rels <- get.relative(nma.linear, nma)

## -----------------------------------------------------------------------------
# Ensure that Suture-absorbable is the network reference
ssi <- ssi_closure %>% dplyr::mutate(agent=factor(trt, levels=c("Suture-absorbable", unique(ssi_closure$trt)[-1])))

# Set dose=0 for network reference and dose=1 for all other interventions
ssi.plac <- ssi %>% dplyr::mutate(dose=dplyr::case_when(trt=="Suture-absorbable" ~ 0,
                                                        TRUE ~ 1))

network.plac <- mbnma.network(ssi.plac)
plot(network.plac)
# Note that Suture-absorbable (the comparator) has been renamed to Placebo

## ----results="hide"-----------------------------------------------------------
# Run linear MBNMA model
nma.linear <- mbnma.run(network.plac, fun=dpoly(degree=1),
                        n.iter=50000)

## -----------------------------------------------------------------------------
summary(nma.linear)

## ----eval=FALSE---------------------------------------------------------------
#  # Random class effect model
#  nma.class <- mbnma.run(network.plac, fun=dpoly(degree=1),
#                         class.effect=list(beta.1="random"),
#                         n.iter=50000)

