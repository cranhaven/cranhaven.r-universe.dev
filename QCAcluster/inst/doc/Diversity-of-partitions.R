## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(QCAcluster)
library(knitr) # nicer html tables

## -----------------------------------------------------------------------------
# load data (see data description for details)
data(Schwarz2016)
Schwarz_div <- partition_div(Schwarz2016, 
                             units = "country", time = "year", 
                             cond = c("poltrans", "ecotrans", "reform", "conflict", "attention"), 
                             out = "enlarge", 1, 0.8)
kable(Schwarz_div)

