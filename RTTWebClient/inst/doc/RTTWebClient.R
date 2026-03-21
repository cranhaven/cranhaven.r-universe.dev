## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=FALSE--------------------------------------------------------
#  library(RTTWebClient)
#  library(lubridate)

## ---- eval=FALSE--------------------------------------------------------------
#  ttWebApiHost <- InitRTTWebApiHost(server = "ttlivewebapi.fxopen.com")

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebApiHost$GetDividends())

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebApiHost$GetSymbolsInfo())

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebApiHost$GetBarsHistory("EURUSD", "Bid","M1", now("UTC") - days(1), now("UTC")))

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebApiHost$GetTickHistory("EURUSD",  now("UTC") - days(1), now("UTC")))

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebApiHost$GetCurrencyInfo())

