## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(casabourse)

## ----cars---------------------------------------------------------------------
#>Affichage des tickers
tickers()

## -----------------------------------------------------------------------------
msi20 <- msi20.data() #>affection de la table MSI20 a la variable ms
msi20 #>afichage des premiers elements de la table

## -----------------------------------------------------------------------------
#>Affichage des données de MASI
ms <- masi.data() #>affection de la table MASI a la variable ms
head(ms) #>afichage des premiers elements de la table

## -----------------------------------------------------------------------------
#>Affichage des cours journaliers de ATTIJARIWAFA BANK entre 01 janvier 2017 et le 14 decembre 2021
daily.data("ATW","01-01-2017","14-12-2021") #>affection de la table a la variable atw


## ----message=FALSE, warning=FALSE---------------------------------------------
get_info('ATW')

## -----------------------------------------------------------------------------
#>Afichage des données par secteur d'activité
bySector()

## -----------------------------------------------------------------------------
#>Afichage les instruments financiers du marché
instruments()

## -----------------------------------------------------------------------------
#>Afichage du palmares de hausse des cours de la journée
today.prizelist('up')

## -----------------------------------------------------------------------------
#>Affichage des données du marché d'aujourd'hui
today.market()

## -----------------------------------------------------------------------------
#>Affichages des transactions de la journée
today.transactions()

## ----include=FALSE------------------------------------------------------------
library(casabourse) 

## -----------------------------------------------------------------------------
#>lecture des donnees de Attijariwafa Bank
atw <- daily.data(ticker = "ATW", from = "01-01-2020", to = "15-12-2021")
#>lecture des donnees de la Banque Populaire du Maroc
bcp <- daily.data(ticker = "BCP", from = "01-01-2020", to = "15-12-2021")

