## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = F-------------------------------------------------------
library(QCAcluster)
library(knitr) # nicer html tables

## -----------------------------------------------------------------------------
# load data (see data description for details)
data(Thiem2011)
# partition data into time series (within-unit) and cross sections (between-unit)
Thiem_pars <- partition_min(
  dataset = Thiem2011,
  units = "country", time = "year",
  cond = c("fedismfs", "homogtyfs", "powdifffs", "comptvnsfs", 
           "pubsupfs", "ecodpcefs"),
  out = "memberfs",
  n_cut = 6, incl_cut = 0.8,
  solution = "P", # parsimonious solution
  BE_cons = c(0.9, 0.8, 0.7, 0.8, 0.85, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8),
  BE_ncut = rep(1, 11),
  WI_cons = c(0.75, 0.8, 0.9, 0.8, 0.85, rep(0.75, 10)),
  WI_ncut = rep(1, 15))
kable(Thiem_pars)

## ---- error = T---------------------------------------------------------------
# load data (see data description for details)
data(Schwarz2016)
# partition data into cross sections
Schwarz_inter <- partition_min_inter(
  Schwarz2016, 
  time = "year", 
  cond = c("poltrans", "ecotrans", "reform", "conflict", "attention"), 
  out = "enlarge", 
  n_cut = 1, incl_cut = 0.8, 
  WI_cons = rep(0.8, 8), BE_cons = c(0.75, 0.75, 0.75, 0.75, 0.75,
                                     0.8, 0.8, 0.8, 0.8, 0.8),
  WI_ncut = rep(1, 8), BE_ncut = rep(1, 10),
  intermediate = c("1", "1", "1", "1", "1"))
kable(Schwarz_inter)

## ---- error = T---------------------------------------------------------------
# load data (see data description for details)
data(Grauvogel2014)
# partition data by sender country (higher-level unit)
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
kable(GS_pars)

