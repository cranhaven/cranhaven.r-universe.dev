## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  eval=rmarkdown::pandoc_available("1.12.3")
)

library(MBNMAtime)
library(rmarkdown)
library(knitr)
library(dplyr)
#load(system.file("extdata", "vignettedata.rda", package="MBNMAtime", mustWork = TRUE))

## ----network.pain-------------------------------------------------------------
# Using the pain dataset
network.pain <- mb.network(osteopain, reference = "Pl_0")
print(network.pain)

## -----------------------------------------------------------------------------
# Prepare data using the alogliptin dataset
network.alog <- mb.network(alog_pcfb, reference = "placebo")

# Plot network
plot(network.alog)

## ---- eval=FALSE--------------------------------------------------------------
#  plot(network.gout, level = "class", remove.loops = TRUE, label.distance = 5)

## ---- echo=FALSE--------------------------------------------------------------
suppressWarnings(plot(network.gout, level = "class", remove.loops = TRUE, label.distance = 5))

## ---- eval=FALSE--------------------------------------------------------------
#  plot(network.gout, level = "treatment", v.color = "class", label.distance = 5)

## ---- echo=FALSE--------------------------------------------------------------
suppressWarnings(plot(network.gout, level = "treatment", v.color = "class", label.distance = 5))

## ----pain.time----------------------------------------------------------------
# Prepare data using the pain dataset
network.pain <- mb.network(osteopain, reference="Pl_0")

# Draw plot of raw study responses over time
timeplot(network.pain)

## ----obese.time, message=FALSE------------------------------------------------
# Draw plot of within-study relative effects over time grouped by class
network.gout <- mb.network(goutSUA_CFBcomb)
timeplot(network.gout, level="class", plotby="rel")

## ---- pain.binplot, results="hide"--------------------------------------------
# Plot results for NMAs performed between 0-5, 5-10, 10-15 and 15-26 weeks
binplot(network.pain, overlay.nma=c(0,5,10,15,26))

