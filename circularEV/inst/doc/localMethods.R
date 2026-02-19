## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
        echo=TRUE, results='hold', warning=F, cache=F, 
  #dev = 'pdf', 
  message=F, 
  fig.width=5, fig.height=5, # fig.retina=0.7,
  tidy.opts=list(width.cutoff=75), tidy=TRUE
)
old <- options(scipen = 1, digits = 4)

## ----setup--------------------------------------------------------------------
library(circularEV)
require(plotly)

## -----------------------------------------------------------------------------
data(HsSP)
data(drc)
timeRange <- 54.5

idx <- order(drc)
drc <- drc[idx]
Data <- HsSP[idx]
set.seed(1234)
Data <- Data + runif(length(Data), -1e-4, 1e-4)

## -----------------------------------------------------------------------------
PlotData(Data=Data, drc=drc, thr=NULL, pointSize=1, cex.axis=15, cex.lab=2, thrWidth=2)
PolarPlotData(Data=Data, drc=drc, thr=NULL, pointSize=4, fontSize=14,
              thrWidth=4, ylim=c(0,max(Data)) )

## -----------------------------------------------------------------------------
thetaVec <- 1:360

## ---- eval=F------------------------------------------------------------------
#  thrResultMom <- ThrSelection(Data=Data, drc=drc, h=60, b=0.35, thetaGrid=thetaVec,
#                            EVIestimator="Mom", useKernel=T, concent=10, bw=30, numCores=2)$thr

## ---- echo=F------------------------------------------------------------------
data(thresholdExampleMom)
thrResultMom <- thresholdExampleMom

## -----------------------------------------------------------------------------
PlotData(Data=Data, drc=drc, thr=thrResultMom, pointSize=1, cex.axis=15, cex.lab=2, thrWidth=2)

PolarPlotData(Data=Data, drc=drc, thr=thrResultMom, pointSize=4, fontSize=12, 
              thrWidth=4, ylim=c(0,max(Data)))

## ---- results='hide'----------------------------------------------------------
h <- 60
useKernel <- TRUE
concent <- 10
movThr <- TRUE
nBoot <- 30
set.seed(1234)
output <- LocalEstim(Data=Data, drc=drc, thr=thrResultMom,
                     thetaGrid=thetaVec, nBoot=nBoot, EVIestimator="Mom", h=h,
                     useKernel=useKernel, concent=concent, movThr=movThr,
                     TTs=c(100, 10000), timeRange=timeRange)

RLBoot <- output$RLBoot

## -----------------------------------------------------------------------------
PlotParamEstim(bootEstimates=output$xiBoot, thetaGrid=thetaVec, ylab=bquote(hat(xi)),
               alpha=0.05, ylim=NULL, cex.axis=15, cex.lab=2, thrWidth=2)

PlotParamEstim(bootEstimates=output$sigBoot, thetaGrid=thetaVec, ylab=bquote(hat(sigma)),
               alpha=0.05, ylim=NULL, cex.axis=15, cex.lab=2, thrWidth=2)

## -----------------------------------------------------------------------------
# 100-year level

PlotRL(RLBootList=RLBoot, thetaGrid=thetaVec, Data=Data, drc=drc,
       TTs=c(100, 10000), whichPlot=1, alpha=0.05, ylim=NULL,
       pointSize=1, cex.axis=15, cex.lab=2, thrWidth=2)

PolarPlotRL(RLBootList=RLBoot, thetaGrid=thetaVec, Data=Data, drc=drc,
            TTs=c(100, 10000), whichPlot=1, alpha=0.05, ylim=NULL,
            pointSize=4, fontSize=12, lineWidth=2)

## -----------------------------------------------------------------------------
# 10000-year level

PlotRL(RLBootList=RLBoot, thetaGrid=thetaVec, Data=Data, drc=drc,
       TTs=c(100, 10000), whichPlot=2, alpha=0.05, ylim=NULL,
       pointSize=1, cex.axis=15, cex.lab=2, thrWidth=2)

PolarPlotRL(RLBootList=RLBoot, thetaGrid=thetaVec, Data=Data, drc=drc,
            TTs=c(100, 10000), whichPlot=2, alpha=0.05, ylim=NULL,
            pointSize=4, fontSize=12, lineWidth=2)

## ---- include = FALSE---------------------------------------------------------
options(old)

