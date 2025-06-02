## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message=FALSE,
  warning = FALSE,
  comment = "#>"
)

## ----echo = FALSE-------------------------------------------------------------
if (!requireNamespace("rmarkdown") || !rmarkdown::pandoc_available("1.12.3")) {
  warning("This vignette requires pandoc version 1.12.3; code will not run in older versions.")
  knitr::opts_chunk$set(eval = FALSE)
}

## ---- fig.dim = c(7, 4)-------------------------------------------------------
#library(mrf)
data("entsoe", package="mrf")
UnivariateData = entsoe$value
plot(UnivariateData, type = "l", xlab="Days", ylab="Electricity Demand MW/h", col = 4)

## ---- fig.dim = c(7, 4)-------------------------------------------------------
dec = mrf::wavelet_decomposition(UnivariateData = UnivariateData, Aggregation = c(2,4))
plot(dec$WaveletCoefficients[1,2:length(dec$SmoothCoefficients[1,])], type = "l", main = "Wavelet level 1", xlab="Days", ylab="Electricity Demand MW/h", col = 4)

## ---- fig.dim = c(7, 4)-------------------------------------------------------
plot(dec$WaveletCoefficients[2,4:length(dec$WaveletCoefficients[1,])], type = "l", main = "Wavelet level 2", xlab="Days", ylab="Electricity Demand MW/h",  col = 4)

## ---- fig.dim = c(7, 4)-------------------------------------------------------
plot(dec$SmoothCoefficients[2,4:length(dec$SmoothCoefficients[1,])], type = "l", main = "Last smooth part level", xlab="Days", ylab="Electricity Demand MW/h",  col = 4)

## ---- eval = FALSE------------------------------------------------------------
#  len_data = length(UnivariateData)
#  Train1    = UnivariateData[1:(len_data)]
#  Test1     = UnivariateData[len_data]
#  # One-step forecast (Multiresolution Forecast)
#  model1   = mrf_train(Train1)
#  one_step = mrf_forecast(model1, Horizon=1)
#  Erro1    = one_step$Forecast - Test1

## ---- eval = FALSE------------------------------------------------------------
#  Train2    = UnivariateData[1:(len_data-2)]
#  Test2     = UnivariateData[(len_data-1):len_data]
#  # Multi-step forecast (Multiresolution Forecast)
#  # Horizon = 2 => Forecast with Horizon 1 and 2 as vector
#  model2    = mrf_train(Train2, Horizon=2)
#  multi_step = mrf_forecast(model2, Horizon=2)
#  Error2     = multi_step$Forecast - Test2

## ---- eval = FALSE------------------------------------------------------------
#  CoefficientCombination = c(10,10,10)
#  Aggregation = c(2,4)
#  rw_forecasts = mrf::mrf_rolling_forecasting_origin(UnivariateData,
#                                                     CoefficientCombination,
#                                                     Aggregation,
#                                                     Horizon = 2,
#                                                     Window = 3,
#                                                     Method = "r",
#                                                     NumClusters = 1)
#  Error = rw_forecasts$Error
#  Forecast = rw_forecasts$Forecast
#  MAE = sum(abs(Error))/(dim(Error)[1] * dim(Error)[2]) # Mean Absolute Error

