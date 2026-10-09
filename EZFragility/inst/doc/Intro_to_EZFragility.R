## ----setup, include = FALSE---------------------------------------------------
library(EZFragility)
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.align = "center",
    fig.width = 6,
    fig.height = 4
)

## ----ictal data---------------------------------------------------------------
data("pt01EcoG")

# option 1: boolean value indicating if the electrode is in the SOZ
rowData(pt01EcoG)$soz

# option 2: SOZ names from the metadata
sozNames <- metaData(pt01EcoG)$sozNames
sozNames

## -----------------------------------------------------------------------------
plot(pt01EcoG)

## -----------------------------------------------------------------------------
display <- c(sozNames, "MLT1", "MLT2", "MLT3", "MLT4")
plot(pt01EcoG[display])

## -----------------------------------------------------------------------------
plot(pt01EcoG[display, 1:100])

## -----------------------------------------------------------------------------
epochClipped <- crop(pt01EcoG, start = -1, end = 0)

plot(epochClipped)

## -----------------------------------------------------------------------------
## Register a SNOW parallel backend with 4 workers
library(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

windowNum <- 250
step <- 125
pt01Frag <- calcAdjFrag(epoch = pt01EcoG, window = windowNum, step = step, parallel = TRUE, nSearch=100L,progress = TRUE)

# Fragility result
pt01Frag

# Stop the parallel backend
stopCluster(cl)

## -----------------------------------------------------------------------------
soz <- estimateSOZ(pt01Frag[ ,pt01Frag$startTimes > 0])
soz

## -----------------------------------------------------------------------------
stats <- fragStat(pt01Frag, soz)
stats

## -----------------------------------------------------------------------------
display <- c(sozNames, "MLT1", "MLT2", "MLT3", "MLT4")
plot(pt01Frag[display], groupIndex = sozNames)

## ----out.width="100%"---------------------------------------------------------
plotFragDistribution(pt01Frag[display], groupIndex = sozNames)

## ----out.width="100%"---------------------------------------------------------
plotFragQuantile(pt01Frag[display], groupIndex = sozNames, groupName = "SOZ")

