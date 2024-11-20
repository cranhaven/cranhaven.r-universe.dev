## ----setup, include = FALSE---------------------------------------------------
library(MBNMAdose)
#devtools::load_all()
library(rmarkdown)
library(knitr)
library(dplyr)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  include=TRUE,
  tidy.opts=list(width.cutoff=80),
  tidy=TRUE
)

## ----results="hide", warning=FALSE, message=FALSE-----------------------------
# Using the alogliptin dataset
alognet <- mbnma.network(alog_pcfb)
nma <- nma.run(alognet, method="random")
ume <- nma.run(alognet, method="random", UME = TRUE)

## ----echo=FALSE---------------------------------------------------------------
kable(data.frame(
  "Model"=c("NMA", "UME"),
  "resdev"=c(nma$jagsresult$BUGSoutput$median$totresdev,
                        ume$jagsresult$BUGSoutput$median$totresdev),
  "sd"=c(MBNMAdose:::neatCrI(nma$jagsresult$BUGSoutput$summary[rownames(nma$jagsresult$BUGSoutput$summary)=="sd", c(3,5,7)], digits = 2),
                       MBNMAdose:::neatCrI(ume$jagsresult$BUGSoutput$summary[rownames(ume$jagsresult$BUGSoutput$summary)=="sd", c(3,5,7)], digits=2))
), digits=2,
col.names=c("Model", "Residual Deviance", "Betwen-study SD"))

## ----results="hide", warning=FALSE, message=FALSE-----------------------------
# Compares residual deviance contributions from NMA and UME models
devdev(nma, ume, dev.type="resdev")

## ----results="hide", warning=FALSE, message=FALSE, fig.show = "hide", eval=FALSE----
#  # Using the psoriasis dataset (>75% improvement in PASI score)
#  psoriasis$r <- psoriasis$r75
#  psorinet <- mbnma.network(psoriasis)
#  
#  # Identify comparisons on which node-splitting is possible
#  splitcomps <- inconsistency.loops(psorinet$data.ab, incldr=TRUE)
#  print(splitcomps)
#  
#  # If we want to fit an Emax dose-response function, there is insufficient
#  #indirect evidence in all but the first 6 comparisons
#  nodesplit <- mbnma.nodesplit(psorinet, fun=demax(), comparisons=splitcomps[1:6,], method="common")

## ----eval=FALSE---------------------------------------------------------------
#  print(nodesplit)

## ----eval=FALSE---------------------------------------------------------------
#  # Plot forest plots of direct, indirect and pooled (MBNMA) results for each comparison
#  plot(nodesplit, plot.type="forest")
#  
#  # Plot posterior densities of direct and indirect results for each nodesplit comparisons
#  plot(nodesplit, plot.type="density")

