## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    comment = "#",
    echo=FALSE,
  error = FALSE,
  tidy = FALSE,
  cache = FALSE,
  collapse = TRUE,
  eval=FALSE, # dont rerun vignette when building package
  out.width = '100%',
  dpi = 144
)

## ----include=FALSE------------------------------------------------------------
#  
#  library(pracma)
#  library(parallel)
#  library(stats)
#  library(maps)
#  library(data.table)
#  library(CVXR)
#  library(DiSCos)
#  library(ggplot2)

## -----------------------------------------------------------------------------
#  data("dube")
#  head(dube)

## -----------------------------------------------------------------------------
#  id_col.target <- 2
#  t0 <- 2003

## ----fig.width=8,fig.height=5-------------------------------------------------
#  df <- copy(dube)
#  disco <- DiSCo(df, id_col.target, t0, G = 1000, num.cores = 1, permutation = TRUE, CI = TRUE, boots = 1000, graph = TRUE, simplex=TRUE, seed=1, q_max=0.9)

## -----------------------------------------------------------------------------
#  # retrieve the weights
#  weights <- disco$weights
#  
#  # retrieve the control unit IDs
#  controls <- disco$control_ids
#  
#  # store in a dataframe
#  weights_df <- data.frame(weights = weights, fips = controls)
#  
#  # merge with state fips codes (built into the maps package)
#  state.fips <- as.data.table(maps::state.fips)
#  state.fips <- state.fips[!duplicated(state.fips$abb), c("fips", "abb")]
#  weights_df <- merge(weights_df, state.fips, by = "fips")
#  
#  setorder(weights_df, -weights)
#  
#  print(weights_df[1:10,])
#  

## -----------------------------------------------------------------------------
#  summary(disco$perm)

## ----fig.width=5,fig.height=8, fig.align='center'-----------------------------
#  discot <- DiSCoTEA(disco,  agg="quantileDiff", graph=TRUE)
#  summary(discot)

## ----fig.width=5,fig.height=8, fig.align='center'-----------------------------
#  discot <- DiSCoTEA(disco,  agg="cdfDiff", graph=TRUE, ylim=c(-0.05, 0.05))
#  summary(discot)

## -----------------------------------------------------------------------------
#  stats::ecdf(disco$results.periods$`2000`$target$quantiles)(3.5)

## -----------------------------------------------------------------------------
#  disco <- DiSCo(dube, id_col.target=id_col.target, t0=t0, G = 1000, num.cores = 1, permutation = TRUE, CI = TRUE, boots = 1000, graph = FALSE, q_min = 0, q_max=0.65, seed=1, simplex=TRUE)

## ----fig.width=5,fig.height=8, fig.align='center'-----------------------------
#  discot <- DiSCoTEA(disco, agg="quantileDiff", graph=TRUE)
#  summary(discot)
#  

## ----fig.width=5,fig.height=8, fig.align='center'-----------------------------
#  discot <- DiSCoTEA(disco, agg="cdfDiff", graph=TRUE, ylim=c(-0.05,0.05))
#  summary(discot)

