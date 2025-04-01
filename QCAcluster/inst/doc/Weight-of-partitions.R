## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = F, warning = F------------------------------------------
library(QCAcluster)
library(knitr) # nicer html tables

## -----------------------------------------------------------------------------
# load data (see data description for details)
data("Thiem2011")
# calculate weight of partitions
wop_pars <- wop(
  dataset = Thiem2011,
  units = "country", time = "year",
  cond = c("fedismfs", "homogtyfs", "powdifffs", "comptvnsfs", "pubsupfs", "ecodpcefs"),
  out = "memberfs",
  n_cut = 6, incl_cut = 0.8,
  solution = "P",
  amb_selector = 1)
kable(wop_pars)

## -----------------------------------------------------------------------------
# sum over all cross-sections for consistency denominator
sum(wop_pars[wop_pars$type == "between", ]$denom_cons)
# sum over all time series for coverage  numerator
sum(wop_pars[wop_pars$type == "within", ]$num_cov)

## -----------------------------------------------------------------------------
# relative contribution of cross sections to denominator for consistency
wop_between  <- wop_pars[wop_pars$type == "between", ]
wop_between$rel_denom_cons <- round(wop_between$denom_cons / 
  sum(wop_between$denom_cons), digits = 2)
kable(wop_between)

## ---- eval = F----------------------------------------------------------------
#  # load data (see data description for details)
#  data("Schwarz2016")
#  # calculating weight of partitions
#  Schwarz_wop_inter <- partition_min_inter(
#    Schwarz2016,
#    units = "country", time = "year",
#    cond = c("poltrans", "ecotrans", "reform", "conflict", "attention"),
#    out = "enlarge",
#    n_cut = 1, incl_cut = 0.8,
#    intermediate = c("1", "1", "1", "1", "1"))
#  kable(Schwarz_wop_inter)

