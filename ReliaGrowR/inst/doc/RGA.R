## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ReliaGrowR)

## -----------------------------------------------------------------------------
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)

## -----------------------------------------------------------------------------
result <- rga(times, failures)
plot(result, main = "Crow-AMSAA Model", xlab = "Cumulative Time", ylab = "Cumulative Failures")

## -----------------------------------------------------------------------------
times <- c(25, 55, 97, 146, 201, 268, 341, 423, 513, 609, 710, 820, 940, 1072, 1217)
failures <- c(1, 1, 2, 4, 4, 1, 1, 2, 1, 4, 1, 1, 3, 3, 4)
breaks <- 500

## -----------------------------------------------------------------------------
result <- rga(times, failures, model_type = "Piecewise NHPP", breaks = c(breaks))
plot(result, main = "Piecewise NHPP Model", xlab = "Cumulative Time", ylab = "Cumulative Failures")

## -----------------------------------------------------------------------------
times <- c(25, 55, 97, 146, 201, 268, 341, 423, 513, 609, 710, 820, 940, 1072, 1217)
failures <- c(1, 1, 2, 4, 4, 1, 1, 2, 1, 4, 1, 1, 3, 3, 4)

## -----------------------------------------------------------------------------
result <- rga(times, failures, model_type = "Piecewise NHPP")
plot(result, main = "Piecewise NHPP with Change Point Detection", xlab = "Cumulative Time", ylab = "Cumulative Failures")

## -----------------------------------------------------------------------------
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)

## -----------------------------------------------------------------------------
fit <- duane(times, failures)
plot(fit, main = "Duane Plot", xlab = "Cumulative Time", ylab = "Cumulative MTBF")

## ----echo=TRUE----------------------------------------------------------------
times <- c(50, 100, 150, 200, 300, 400, 600, 800, 1000)
failures <- c(1, 1, 2, 2, 3, 4, 5, 6, 8)
result <- rga(times, failures)
qqplot.rga(result)

## ----echo=TRUE----------------------------------------------------------------
ppplot.rga(result)

## ----echo=TRUE----------------------------------------------------------------
failures <- c(100, 200, 200, 400)
suspensions <- c(250, 350, 450)
interval_starts <- c(150, 300)
interval_ends <- c(180, 320)

## ----echo=TRUE----------------------------------------------------------------
result <- weibull_to_rga(failures, suspensions, interval_starts, interval_ends)

## ----echo=TRUE----------------------------------------------------------------
fit <- rga(result$CumulativeTime, result$Failures)
plot(fit, main = "RGA from Converted Data", xlab = "Cumulative Time", ylab = "Cumulative Failures")

