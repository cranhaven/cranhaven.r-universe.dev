## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(QCAcluster)

## ---- fig.width=6, fig.height=4-----------------------------------------------
data("Grauvogel2014")
# parsimonious solution for each type of Sender
GS_pars <- partition_min(
 dataset = Grauvogel2014,
 units = "Sender",
 cond = c("Comprehensiveness", "Linkage", "Vulnerability",
          "Repression", "Claims"),
 out = "Persistence",
 n_cut = 1, incl_cut = 0.75,
 solution = "P",
 BE_cons = rep(0.75, 3),
 BE_ncut = rep(1, 3))
# UpSet plot with three sets
upset_conditions(GS_pars, nsets = 4)

## ---- fig.width=6, fig.height=4-----------------------------------------------
upset_configurations(GS_pars, nsets = 3)

