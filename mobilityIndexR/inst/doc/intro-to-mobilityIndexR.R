## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(mobilityIndexR)

## ----data---------------------------------------------------------------------
head(incomeMobility)

## ----relative_probsF----------------------------------------------------------
getTMatrix(dat = incomeMobility, col_x = "t0", col_y = "t5", 
           type = "relative", num_ranks = 5, probs = FALSE)

## ----relative_probsT----------------------------------------------------------
getTMatrix(dat = incomeMobility, col_x = "t0", col_y = "t5", 
           type = "relative", num_ranks = 5, probs = TRUE)

## ----mixed--------------------------------------------------------------------
getTMatrix(dat = incomeMobility, col_x = "t0", col_y = "t5", 
           type = "mixed", num_ranks = 5, probs = FALSE)

## ----absolute-----------------------------------------------------------------
getTMatrix(dat = gradeMobility, col_x = "t0", col_y = "t5", 
           type = "absolute", probs = FALSE, bounds = c(0, 0.6, 0.7, 0.8, 0.9, 1.0))

## ----indices------------------------------------------------------------------
getMobilityIndices(dat = incomeMobility, col_x = "t0", col_y = "t5", 
                   type = "relative", num_ranks = 5)

## ----one_index----------------------------------------------------------------
getMobilityIndices(dat = incomeMobility, col_x = "t0", col_y = "t5", 
                   type = "relative", num_ranks = 5, indices = "wgm")

## ----hypothesis_test----------------------------------------------------------
getHypothesisTest(dat_A = incomeMobility, dat_B = incomeMobility, 
                  cols_A = c("t0", "t3"), cols_B = c("t5", "t8"),
                  type = "relative", num_ranks = 5, bootstrap_iter = 100)

## ----indices_intervals--------------------------------------------------------
getMobilityIndices(dat = incomeMobility, col_x = "t0", col_y = "t5", 
                   type = "relative", num_ranks = 5, indices = "wgm",
                   intervals = TRUE, interval_pct = 0.95, bootstrap_iter = 100)

## ----matrices_error, error=TRUE-----------------------------------------------
getTMatrix(dat = incomeZeroInfMobility, col_x = "t0", col_y = "t5",
           type = "relative", num_ranks = 5, probs = FALSE)
table(incomeZeroInfMobility$t0)

## ----matrices_strict, error=TRUE----------------------------------------------
getTMatrix(dat = incomeZeroInfMobility, col_x = "t0", col_y = "t5",
           type = "relative", num_ranks = 5, probs = FALSE, strict = FALSE)

## ----matrices_exclude_value, error=TRUE---------------------------------------
getTMatrix(dat = incomeZeroInfMobility, col_x = "t0", col_y = "t5",
           type = "relative", num_ranks = 5, probs = FALSE,
           exclude_value = 0, rerank_exclude_value = "as_new_rank")

getTMatrix(dat = incomeZeroInfMobility, col_x = "t0", col_y = "t5",
           type = "relative", num_ranks = 5, probs = FALSE,
           exclude_value = 0, rerank_exclude_value = "as_existing_rank")

getTMatrix(dat = incomeZeroInfMobility, col_x = "t0", col_y = "t5",
           type = "relative", num_ranks = 5, probs = FALSE,
           exclude_value = 0, rerank_exclude_value = "exclude")

