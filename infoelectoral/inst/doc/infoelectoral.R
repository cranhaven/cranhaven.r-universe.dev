## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = T, results = T, include = T, warning = F, message = F)

## -----------------------------------------------------------------------------
library(infoelectoral)
library(dplyr)
df <- municipios(tipo_eleccion = "congreso", anno = "1982", mes = "10")
glimpse(df)

## -----------------------------------------------------------------------------
df <- mesas("congreso", "2019", "04")
glimpse(df)

## -----------------------------------------------------------------------------
df <- candidatos(tipo_eleccion = "senado", anno = "2019", mes = "11", nivel = "municipio")
glimpse(df)

## -----------------------------------------------------------------------------
df <- candidatos("europeas", "2019", "05")
glimpse(df)

