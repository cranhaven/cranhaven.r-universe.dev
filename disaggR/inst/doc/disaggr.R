## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.show="hold",
  fig.width = 7,
  fig.height = 3,
  out.width = "98%",
  dpi = 150
)

## ----echo=FALSE---------------------------------------------------------------
library(disaggR)

## ----eval=FALSE---------------------------------------------------------------
#  twoStepsBenchmark(turnover,construction)

## ----eval=FALSE---------------------------------------------------------------
#  threeRuleSmooth(turnover,construction)

## -----------------------------------------------------------------------------
benchmark <- twoStepsBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
plot(in_sample(benchmark,type="levels"),
     start=c(2010,1),end=c(2017,1))

## -----------------------------------------------------------------------------
library(ggplot2)
smooth <- threeRuleSmooth(hfserie = turnover,
                          lfserie = construction)
autoplot(in_disaggr(smooth),
         start=c(2009,1),end=c(2013,12),
         show.legend = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  benchmark <- twoStepsBenchmark(turnover,construction)
#  smooth <- threeRuleSmooth(turnover,construction)
#  
#  reView(benchmark)
#  rePort(benchmark)
#  
#  as.ts(benchmark);as.ts(smooth)
#  as.list(benchmark);as.list(smooth)
#  coef(benchmark)
#  residuals(benchmark)
#  vcov(benchmark)
#  fitted(benchmark)
#  model.list(benchmark);model.list(smooth)
#  se(benchmark)
#  rho(benchmark)
#  outliers(benchmark)
#  smoothed.rate(smooth)
#  summary(benchmark)

