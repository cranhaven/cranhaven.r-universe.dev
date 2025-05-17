## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(smiles)

## ----setup, echo = FALSE, warning = FALSE, message = FALSE--------------------
library(smiles)
library(meta)

## ----eval = FALSE-------------------------------------------------------------
#  library(meta)
#  data("Fleiss1993bin")
#  dataFleiss1993bin <- Fleiss1993bin

## ----eval = FALSE-------------------------------------------------------------
#  DoTSA(Fleiss1993bin,
#       source = study,
#       time = year,
#       r1 = d.asp,
#       n1 = n.asp,
#       r2 = d.plac,
#       n2 = n.plac,
#       measure = "RR",
#       PES = 0.1,
#       RRR = 0.2,
#       group = c("Aspirin", "Placebo"))

## ----result-DoTSA, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
DoTSA(Fleiss1993bin, 
     source = study, 
     time = year,
     r1 = d.asp, 
     n1 = n.asp, 
     r2 = d.plac, 
     n2 = n.plac, 
     measure = "RR",
     PES = 0.1,
     RRR = 0.2,
     group = c("Aspirin", "Placebo"))

## ----eval = FALSE-------------------------------------------------------------
#  DoTSA(Fleiss1993bin,
#       source = study,
#       time = year,
#       r1 = d.asp,
#       n1 = n.asp,
#       r2 = d.plac,
#       n2 = n.plac,
#       measure = "RR",
#       PES = 0.1,
#       RRR = 0.2,
#       group = c("Aspirin", "Placebo"),
#       plot = TRUE)

## ----plot-sequential, fig.cap = "An example for sequential analysis", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
DoTSA(Fleiss1993bin, 
     source = study, 
     time = year,
     r1 = d.asp, 
     n1 = n.asp, 
     r2 = d.plac, 
     n2 = n.plac, 
     measure = "RR",
     RRR = 0.2,
     group = c("Aspirin", "Placebo"),
     plot = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  PlotCoRNET(output,
#             lgcZone = TRUE,
#             lgcLblStdy = TRUE,
#             anglStdy = 60)

## ----plot-CoRNET, fig.cap = "An example for illustrating colorful zones on trial sequential analysis", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 8, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
output <- DoTSA(Fleiss1993bin, 
     source = study, 
     time = year,
     r1 = d.asp, 
     n1 = n.asp, 
     r2 = d.plac, 
     n2 = n.plac, 
     measure = "RR",
     RRR = 0.2,
     group = c("Aspirin", "Placebo"),
     SAP = TRUE,
     plot = FALSE)

PlotCoRNET(output,
           lgcZone = TRUE,
           lgcLblStdy = TRUE,
           anglStdy = 60)

