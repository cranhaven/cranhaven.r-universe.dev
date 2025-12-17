## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(ichimoku)

## ----ichimoku-----------------------------------------------------------------
# Simulated OHLC pricing data is assigned to data frame 'TKR':
TKR <- sample_ohlc_data
cloud <- ichimoku(TKR)

## ----strat--------------------------------------------------------------------
strat <- strat(cloud, c1 = "cloudB", c2 = "kijun")
print(strat[100:105, ], plot = FALSE)

## ----summary------------------------------------------------------------------
summary(strat)

## ----plot---------------------------------------------------------------------
plot(strat, theme = "dark")

## ----combine------------------------------------------------------------------
strat2 <- strat(cloud, "kijun", "tenkan")

newstrat <- stratcombine(strat, strat2)
summary(newstrat)

## ----perfana, eval=FALSE------------------------------------------------------
# library(PerformanceAnalytics)
# # To chart performance comparison of strategy vs benchmark, daily returns and drawdowns
# charts.PerformanceSummary(strat[, c("sret", "ret")])
# # For a table detailing drawdowns, including depth, length of recovery etc.
# table.Drawdowns(strat[, "sret"])

## ----autostrat----------------------------------------------------------------
autostrat(cloud, n = 3)

## ----autostrat2---------------------------------------------------------------
autostrat(cloud, n = 3, dir = "short", level = "2")

## ----autostrat3---------------------------------------------------------------
autostrat(cloud, n = 3, dir = "long", level = "3")

## ----mlgrid-------------------------------------------------------------------
mlgrid(cloud, y = "logret", dir = "long", type = "boolean", unique = TRUE)[100:105, 1:4]

## ----mlgrid2------------------------------------------------------------------
mlgrid(cloud, y = "ret", dir = "short", type = "numeric", unique = FALSE)[100:105, 1:4]

## ----mlgrid3------------------------------------------------------------------
mlgrid(cloud, y = "ret", dir = "short", type = "z-score", unique = FALSE)[100:105, 1:4]

## ----relative-----------------------------------------------------------------
relative(cloud, signif = 0.4)[1:10, ]

