## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  library("SC.MEB")

## ----eval=FALSE---------------------------------------------------------------
#  file = system.file("extdata", "CRC3.rds", package = "SC.MEB")
#  CRC = readRDS(file)

## ----eval=FALSE---------------------------------------------------------------
#  set.seed(114)
#  library(scuttle)
#  library(scran)
#  library(scater)
#  library(BiocSingular)
#  CRC <- spatialPreprocess(CRC, platform="Visium")

## ----eval=FALSE---------------------------------------------------------------
#  platform = "Visium"
#  beta_grid = seq(0,4,0.2)
#  K_set= 2:10
#  parallel=TRUE
#  num_core = 3
#  PX = TRUE
#  maxIter_ICM = 10
#  maxIter = 50

## ----eval=FALSE---------------------------------------------------------------
#  library(SingleCellExperiment)
#  Adj_sp  <- find_neighbors2(CRC, platform = "Visium")
#  Adj_sp[1:10,1:10]

## ----eval=FALSE---------------------------------------------------------------
#  y = reducedDim(CRC, "PCA")[,1:15]
#  fit = SC.MEB(y, Adj_sp, beta_grid = beta_grid, K_set= K_set, parallel=parallel, num_core = num_core, PX = PX, maxIter_ICM=maxIter_ICM, maxIter=maxIter)
#  str(fit[,1])

## ----eval=FALSE---------------------------------------------------------------
#  selectKPlot(fit, K_set = K_set, criterion = "BIC")

## ----eval=FALSE---------------------------------------------------------------
#  selectKPlot(fit, K_set = K_set, criterion = "MBIC")

## ----eval=FALSE---------------------------------------------------------------
#  out = selectK(fit, K_set = K_set, criterion = "BIC")
#  pos = matrix(cbind(colData(CRC)[,c(4)],20000-colData(CRC)[,c(3)]), 2988, 2)
#  ClusterPlot(out, pos, size = 3, shape = 16)

## ----eval=FALSE---------------------------------------------------------------
#  out = selectK(fit, K_set = K_set, criterion = "MBIC")
#  pos = matrix(cbind(colData(CRC)[,c(4)],20000-colData(CRC)[,c(3)]), 2988, 2)
#  ClusterPlot(out, pos, size = 3, shape = 16)

