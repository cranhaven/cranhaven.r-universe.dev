## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(multiDoE)

## -----------------------------------------------------------------------------
backup_options <- options()

set.seed(13)
options(digits = 15)

facts <- list(1, 2:5)
units <- list(21, 2)
levels <- 3
etas <- list(1)
criteria <- c('Id', 'Ds')
model <- "quadratic"

## -----------------------------------------------------------------------------
iters <- 5 * length(criteria)
restarts <- 10
restInit <- 2

## -----------------------------------------------------------------------------
tpls <- runTPLS(facts,units, criteria, model, iters, 
                "Restarts", restarts, 
                "RestInit", restInit)

## -----------------------------------------------------------------------------
plotPareto(tpls$megaAR)

## -----------------------------------------------------------------------------
optMultiCrit(tpls$megaAR)

## -----------------------------------------------------------------------------
topsis_solutions <- topsisOpt(tpls)

topsis_solutions$bestScore

## -----------------------------------------------------------------------------
topsis_solutions$bestSol

## -----------------------------------------------------------------------------
optSingleCrit(tpls$megaAR)

## -----------------------------------------------------------------------------
options(backup_options)

