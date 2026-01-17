## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simulateDCE)
library(rlang)
library(formula.tools)

## ----initialize---------------------------------------------------------------
bcoeff <- list(
  bx1 = -0.1,
  bx2 = -0.1,
  bx3 = -0.05,
  bx4 = -0.025
)

# place your utility functions here
ul <- list(u1 = list(
  v1 = V.1 ~ bx1 * alt1.x1 + bx2 * alt1.x2 + bx3 * alt1.x3 + bx4 * alt1.x4,
  v2 = V.2 ~ bx1 * alt2.x1 + bx2 * alt2.x2 + bx3 * alt2.x3 + bx4 * alt2.x4,
  v3 = V.3 ~ -5
))

## ----other--------------------------------------------------------------------
designpath <- system.file("extdata", "CSA", "linear", package = "simulateDCE")
## can also be specified using relative path eg. designpath<- "Projects/CSA/Designs/"

# notes <- "This design consists of different heuristics. One group did not attend the methan attribute, another group only decided based on the payment"

notes <- "No Heuristics"

resps <- 240 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)

## design type must be either 'spdesign' or 'ngene'
destype <- "spdesign"

## ----random-------------------------------------------------------------------
set.seed(3393)

## ----output-------------------------------------------------------------------
csa <- simulateDCE::sim_all(
  nosim = nosim, resps = resps, designtype = destype,
  designpath = designpath, u = ul, bcoeff = bcoeff
)

## ----accessOutput-------------------------------------------------------------
topLevelResults <- names(csa[sapply(csa, is.list)])

print(topLevelResults)

## saves and prints the key results of the first expreimental design
simulationCoeff <- csa[[1]]$coefs
coeffSummary <- csa[[1]]$summary

print(simulationCoeff)
print(coeffSummary)
## saveRDS(csa,file = "tests/manual-tests/csa.RDS")

