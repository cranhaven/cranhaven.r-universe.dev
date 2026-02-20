## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

## ----library------------------------------------------------------------------
# Install from CRAN 
# install.packages("INetTool")

# Install from GitHub
# install.packages("devtools")
#devtools::install_github("ValeriaPolicastro/INet-Tool")


## ----data---------------------------------------------------------------------
library(INetTool)
data("exampleL_data")

## ----construction-------------------------------------------------------------
net <- constructionGraph(exampleL_data)

## -----------------------------------------------------------------------------
adjL <- net$Adj

## -----------------------------------------------------------------------------
graphL <- net$Graphs

## -----------------------------------------------------------------------------
JWmatrix(graphL)

## -----------------------------------------------------------------------------
JWmean(graphL)

## -----------------------------------------------------------------------------
measures <- measuresNet(graphL, nodes.measures=F)

## -----------------------------------------------------------------------------
measures[[1]] 

## ----plot---------------------------------------------------------------------
plotL(graphL,vertex.cex=.1,vertex.labels.cex=.1, vertex.color = 18)

## ----consensus-function-------------------------------------------------------
Con <- consensusNet (adjL, theta=0.05) 


## -----------------------------------------------------------------------------
Con$graphConsensus 

## -----------------------------------------------------------------------------
Con$Comparison

## -----------------------------------------------------------------------------
Con$similarGraphs

## -----------------------------------------------------------------------------
plotC(Con$graphConsensus,vertex.size=6, vertex.label.cex =0.5,vertex.color = 18)

## ----specific-function--------------------------------------------------------
specificNet(graphL, Con$graphConsensus)

## -----------------------------------------------------------------------------
plotINet(adjL[[1]], Con$graphConsensus, vertex.labels.cex=.01, vertex.size=2)

