## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(EstimateBreed)

data("clima")
clima <- get("clima")[1:150, ]

with(clima,atsum(TMED,crop="maize"))

#Adjusting lower basal temperature manually
with(clima,atsum(TMED,crop="maize",lbt=12))

## ----echo = TRUE, fig.height = 5, fig.width = 10, fig.align = "center", message = F, warning = F----
library(EstimateBreed)
data("pheno")

with(pheno, plast(GEN,TMED,EST,NN,habit="ind",plot=TRUE))

## ----eval = FALSE-------------------------------------------------------------
# # This function requires an internet connection to access the weather API.
# library(EstimateBreed)
# 
# # Forecasting application conditions
# forecast <- tdelta(-53.6969,-28.0638,type=1,days=10)
# forecast
# 
# # Retrospective analysis of application conditions
# retrosp <- tdelta(-53.6969,-28.0638,type=2,days=10,
#                  dates=c("2023-01-01","2023-05-01"),
#                  details=TRUE)
# retrosp

## -----------------------------------------------------------------------------
library(EstimateBreed)

data("aveia")

#General
with(aveia,stind(GEN,MC,MG,index = "ALL",bygen=TRUE))

#Only the desired index
with(aveia,stind(GEN,MC,MG,index = "STI",bygen=TRUE))

## -----------------------------------------------------------------------------

library(EstimateBreed)

# Rust Risk Prediction
data("clima")
with(clima, risk(DY, MO, TMED, RH, disease = "rust"))

