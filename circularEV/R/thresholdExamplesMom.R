#' Threshold for local Moment examples
#'
#' A vector of threshold values at directions 1,...,360. It is used for local Moment examples.
#'
#' @format A vector of 360 values.
#' @details It has been generated as follows: \cr  \cr
#' \preformatted{
#' data(HsSP)
#' data(drc)
#' timeRange <- 54.5
#'
#' idx <- order(drc)
#' drc <- drc[idx]
#' Data <- HsSP[idx]
#' set.seed(1234)
#' Data <- Data + runif(length(Data), -1e-4, 1e-4)
#'
#' thetaVec <- 1:360
#'
#' thresholdExampleMom <- ThrSelection(Data=Data, drc=drc, h=60, b=0.35,
#'                                     thetaGrid=thetaVec, EVIestimator="Mom",
#'                                     useKernel=T, concent=10, bw=30, numCores=2)$thr
#' }
"thresholdExampleMom"





