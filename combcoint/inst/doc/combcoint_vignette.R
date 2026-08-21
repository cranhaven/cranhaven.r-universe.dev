## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(combcoint)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("combcoint")

## ----eval=FALSE---------------------------------------------------------------
# remotes::install_github("Janine-Langerbein/combcoint")

## -----------------------------------------------------------------------------
data("lutkepohl_e1")

## -----------------------------------------------------------------------------
englegranger(linvestment ~ lincome + lconsumption, data = lutkepohl_e1)

## -----------------------------------------------------------------------------
bayerhanck(linvestment ~ lincome + lconsumption, data = lutkepohl_e1)

## -----------------------------------------------------------------------------
bayerhanck(linvestment ~ lincome + lconsumption, data = lutkepohl_e1, lags = 4)

