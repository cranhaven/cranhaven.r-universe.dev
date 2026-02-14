## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(Matrix)
library(trafficCAR)

A <- matrix(0, 4, 4)
A[1,2] <- A[2,1] <- 1
A[2,3] <- A[3,2] <- 1
A[3,4] <- A[4,3] <- 1

Q_icar <- car_precision(A, type = "icar", tau = 1)
Q_icar

## -----------------------------------------------------------------------------
Q_prop <- car_precision(A, type = "proper", rho = 0.9, tau = 2)
Q_prop

## -----------------------------------------------------------------------------
A_iso <- matrix(0, 5, 5)
A_iso[1,2] <- A_iso[2,1] <- 1
A_iso[2,3] <- A_iso[3,2] <- 1
A_iso[3,4] <- A_iso[4,3] <- 1
# node 5 is isolated

# ICAR: warning about isolated node(s)
Q_icar_iso <- car_precision(A_iso, type = "icar", tau = 1)
Q_icar_iso

# Proper CAR: error when isolates are present (with check = TRUE)
try(car_precision(A_iso, type = "proper", rho = 0.9, tau = 1))

## -----------------------------------------------------------------------------
Q_icar_scaled <- intrinsic_car_precision(A, tau = 1, scale = TRUE)
Q_icar_scaled

