## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  message  = FALSE,
  warning  = FALSE
)

## ----setup--------------------------------------------------------------------
library(SLmetrics)

## -----------------------------------------------------------------------------
# 1) seed
set.seed(1903)

# 2) actual values
actual <- rnorm(
    n = 100
)

# 3) predicted values
predicted <- actual + rnorm(n = 100)

# 4) sample weights
weights <- runif(
    n = length(actual)
)

## -----------------------------------------------------------------------------
# 1) calculate unweighted RMSE
rmse(
    actual    = actual,
    predicted = predicted
)

# 2) calculate weighted RMSE
weighted.rmse(
    actual    = actual,
    predicted = predicted,
    w         = weights
)

## -----------------------------------------------------------------------------
# 1) calculate RRMSE
# with mean normalization
rrmse(
    actual = actual,
    predicted = predicted,
    normalization = 0
)

# 2) calculate RRSME
# with range normalization
rrmse(
    actual = actual,
    predicted = predicted,
    normalization = 1
)

# 3) calculate RRSME
# with IQR normalization
rrmse(
    actual = actual,
    predicted = predicted,
    normalization = 2
)

