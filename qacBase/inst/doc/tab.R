## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(qacBase)

## ---- include=TRUE------------------------------------------------------------
tab(cardata, driven_wheels)

## ---- include=TRUE------------------------------------------------------------
tab(cardata, driven_wheels, total=TRUE)

## ---- include=TRUE------------------------------------------------------------
tab(cardata, driven_wheels, total=TRUE, sort=TRUE)

## ---- include=TRUE------------------------------------------------------------
tab(cardata, make, sort = TRUE, na.rm = TRUE, total = TRUE, maxcat=10)

## ---- include=TRUE------------------------------------------------------------
tab(cardata, make,  minp=0.05)

## ---- include=TRUE------------------------------------------------------------
tab(cardata, vehicle_style,  sort=TRUE, plot=TRUE)

tab(cardata, vehicle_style, sort=TRUE, cum=TRUE, plot=TRUE)

