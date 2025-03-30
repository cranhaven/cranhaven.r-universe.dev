## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(RcppPlanc)
library(Matrix)

## ----nmfDense-----------------------------------------------------------------
mat <- matrix(runif(50*100), nrow = 50, ncol = 100)
res <- nmf(mat, k = 10, nCores = 2)

## ----inmfLoadData-------------------------------------------------------------
data("ctrl.sparse")
data("stim.sparse")

## ----inmf, message=FALSE, results='hide'--------------------------------------
res <- inmf(list(ctrl.sparse, stim.sparse), k = 20, lambda = 5, nCores = 2)

## ----h5mat--------------------------------------------------------------------
ctrl.denseH5FilePath <- system.file("extdata/ctrl_dense.h5", package = "RcppPlanc")
ctrl.h5dense <- H5Mat(filename = ctrl.denseH5FilePath, dataPath = "data")
ctrl.h5dense

stim.denseH5FilePath <- system.file("extdata/stim_dense.h5", package = "RcppPlanc")
stim.h5dense <- H5Mat(filename = stim.denseH5FilePath, dataPath = "data")
stim.h5dense

ctrl.sparseH5FilePath <- system.file("extdata/ctrl_sparse.h5", package = "RcppPlanc")
ctrl.h5sparse <- H5SpMat(filename = ctrl.sparseH5FilePath, 
                         valuePath = "scaleDataSparse/data", rowindPath = "scaleDataSparse/indices", colptrPath = "scaleDataSparse/indptr",
                         nrow = nrow(ctrl.sparse), ncol = ncol(ctrl.sparse))
ctrl.h5sparse

stim.sparseH5FilePath <- system.file("extdata/stim_sparse.h5", package = "RcppPlanc")
stim.h5sparse <- H5SpMat(filename = stim.sparseH5FilePath,
                         valuePath = "scaleDataSparse/data", rowindPath = "scaleDataSparse/indices", colptrPath = "scaleDataSparse/indptr",
                         nrow = nrow(stim.sparse), ncol = ncol(stim.sparse))
stim.h5sparse

## ----onlineinmf, message=FALSE, results='hide'--------------------------------
res <- onlineINMF(list(ctrl.h5sparse, stim.h5sparse), k = 20, lambda = 5, minibatchSize = 50, nCores = 2)

