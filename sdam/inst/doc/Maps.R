## ----setup, echo=FALSE, message=FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo=TRUE,error=TRUE)
knitr::opts_chunk$set(comment = "")
library("sdam")

## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------------
options(width = 96)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  install.packages("sdam") # from CRAN
#  devtools::install_github("sdam-au/sdam") # development version
#  devtools::install_github("mplex/cedhar", subdir="pkg/sdam") # legacy version R 3.6.x

## ---------------------------------------------------------------------------------------------
# load and check versions
library(sdam)
packageVersion("sdam")

## ---------------------------------------------------------------------------------------------
# Roman provinces
plot.map(type="rp")

## ---------------------------------------------------------------------------------------------
# senatorial/imperial division
plot.map(type="si", main="Roman Empire (AD 117)")

## ---------------------------------------------------------------------------------------------
# Italian peninsula silhouette
plot.map(x="Ita")

## ---------------------------------------------------------------------------------------------
# Roman province with establishment date
plot.map(x="Bri", date=TRUE)

## ---------------------------------------------------------------------------------------------
# Italian region
plot.map(x="VeH", cap=FALSE, fsize=12)

## ---------------------------------------------------------------------------------------------
# settlements and main roads
plot.map(type="plain", settl=TRUE, roads=TRUE)

## ---------------------------------------------------------------------------------------------
# settlements and main shipping routes
plot.map(type="plain", settl=TRUE, shipr=TRUE)

## ---------------------------------------------------------------------------------------------
# shipwrecks dataset
sw <- system.file("extdata","StraussShipwrecks.csv",package="sdam") |> 
  read.csv(sep=";")

## ---------------------------------------------------------------------------------------------
# variables are checked
colnames(sw)

## ---------------------------------------------------------------------------------------------
# graph of shipwrecks network
sw[, 20:21] |> 
  sdam::cln(level=2, what=c("(",")"), case=1, na.rm=TRUE) |> 
  multiplex::transf(type="toarray") |> 
  multigraph::multigraph(layout="force", seed=123, loops=TRUE, ecol=4, sel="Rome")

