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

## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(turnover_catering)
plot(consumption_catering)

## -----------------------------------------------------------------------------
benchmark <- twoStepsBenchmark(turnover_catering,consumption_catering)

## -----------------------------------------------------------------------------
plot(in_disaggr(benchmark, type="levels-rebased"), start=c(2010,1))

## -----------------------------------------------------------------------------
plot(in_disaggr(benchmark), start=c(2018,1))

## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(residuals(benchmark))
plot(smoothed.part(benchmark))

## -----------------------------------------------------------------------------
plot(in_disaggr(benchmark, type ="contributions"), start=2018)

## -----------------------------------------------------------------------------
outlier <- window(turnover_catering-130, c(2020,1), c(2021,12))
outlier[c(1,2, 20:24)] <- 0
plot(outlier)

## -----------------------------------------------------------------------------
benchmark_out <- twoStepsBenchmark(turnover_catering,consumption_catering, 
                                    outliers = list(AO2020 = outlier))

## -----------------------------------------------------------------------------
coefficients(summary(benchmark_out))

## -----------------------------------------------------------------------------
plot(in_disaggr(benchmark_out, type ="contributions"), start=2018)
plot(in_disaggr(benchmark_out, type="levels-rebased"), start=c(2010,1))

## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(residuals(benchmark_out))
plot(smoothed.part(benchmark_out))

## -----------------------------------------------------------------------------
turnover_catering_2020 <- window(turnover_catering, end = c(2020,6))
consumption_catering_2020 <- window(consumption_catering, end = 2019)

## -----------------------------------------------------------------------------
benchmark_2020 <- twoStepsBenchmark(turnover_catering_2020,consumption_catering_2020)
coefficients(summary(benchmark_2020))

## -----------------------------------------------------------------------------
plot(in_disaggr(benchmark_2020, type ="contributions"), start=2018)
plot(in_disaggr(benchmark_2020), start=c(2010,1))


## -----------------------------------------------------------------------------
benchmark_out_2020 <- twoStepsBenchmark(turnover_catering_2020,consumption_catering_2020, 
                                    outliers = list(AO2020 = outlier),
                                    set.coeff = c(AO2020 = 14)
                                        )

## -----------------------------------------------------------------------------
coefficients(summary(benchmark_out_2020))

## -----------------------------------------------------------------------------
plot(in_disaggr(benchmark_out_2020, type ="contributions"), start=2018)
plot(in_disaggr(benchmark_out_2020, type="levels-rebased"), start=c(2010,1))

## ----eval=FALSE---------------------------------------------------------------
#  AO2020 = c(1,2,1,0) # A one-year AO in 2020, with half the weight in Q2
#                      # and the rest split between Q1 and Q3
#  
#  AO2008T2=c(0,0,3) # A one-quarter AO in the second quarter of 2008,
#                    # which is concentrated in June
#  
#  LS2010 = c(0,1,2,3) # A level shift that start in Q2 2010 and increase
#                      # gradually in Q3 and Q4, and remains at its level afterwards

