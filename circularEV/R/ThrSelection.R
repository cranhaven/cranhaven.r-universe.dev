#' Threshold selection
#'
#' This function selects a moving threshold for circular data using an automatic
#' procedure for selecting the local number of exceedances
#'
#' @param Data Response variable
#' @param drc Directional covariate
#' @param h Bandwidth value
#' @param b Parameter used in the automatic procedure for selection of local
#'   number of exceedances
#' @param thetaGrid Grid values at which the estimation is performed
#' @param EVIestimator It can be either "ML" or "Mom"
#' @param useKernel Logical. If TRUE (default), use kernel to assign weights
#'   depending on the directional distance.
#' @param concent Concentration parameter value for von Mises kernel
#' @param bw Bandwidth parameter value for smoothing the sample path of the
#'   selected threshold
#' @param numCores Number of CPU cores to be used
#'
#' @return List containing the selected threshold and selected number of local
#'   exceedances at each direction in the grid.
#'
#' @details See Konzen, E., Neves, C., and Jonathan, P. (2021). Modeling nonstationary extremes of storm severity: Comparing parametric and semiparametric inference. Environmetrics, 32(4), e2667.
#' @export
#' @seealso \code{\link{PlotData}} and \code{\link{PolarPlotData}} to see how
#' the threshold can be visualised.
#' @examples
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
#' \donttest{
#'
#' thrResultMom <- ThrSelection(Data=Data, drc=drc, h=60, b=0.35, thetaGrid=thetaVec,
#'                              EVIestimator="Mom", useKernel=T, concent=10, bw=30,
#'                              numCores=2)$thr
#' thrResultML <- ThrSelection(Data=Data, drc=drc, h=60, b=0.35, thetaGrid=thetaVec,
#'                             EVIestimator="ML", useKernel=T, concent=10, bw=30,
#'                             numCores=2)$thr
#' }
#'
#'
#' ## See also examples in vignettes:
#' # vignette("localMethods", package = "circularEV")
#' # vignette("splineML", package = "circularEV")
#'
#' @importFrom parallel makePSOCKcluster
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @import foreach
#' @importFrom circular circular
#' @importFrom NPCirc kern.reg.circ.lin
ThrSelection <- function(Data, drc, h=30, b=0.35, thetaGrid, EVIestimator="ML",
                         useKernel=TRUE, concent=10, bw=30, numCores=2){

  if(!all(drc%in%thetaGrid)){
    stop("thetaGrid must include all observed directions (drc)")
  }

  if(numCores > parallel::detectCores() - 2){
    numCores <- parallel::detectCores() - 2
  }
  cat(paste0(numCores, " CPU cores will be used \n"))


  if(length(Data)!=length(drc)){
    stop("excesses and drc must have same length")
  }

  n <- length(Data)

  allRES <- list()
  cl <- makePSOCKcluster(numCores, outfile="")
  registerDoParallel(cl)
  pb <- txtProgressBar(title = "Progress",
                       min = 0, max = length(thetaGrid), style = 3)

  resultsParallel <- foreach(s_i=1:length(thetaGrid), # paralellised loop
                             .multicombine = FALSE,
                             .export = c("AutoChoice_k_circ", "MomEst", "MLEst",
                                         "MomEstKernel", "MLEstKernel", "myKernel",
                                         "kernelnegloglik")) %dopar% {

  setTxtProgressBar(pb, s_i)

  # for(s_i in ssVec){  # not parallised code
     ss <- thetaGrid[s_i]

     whichNeighb <- rep(NA, n)
     for(i in 1:n){
       whichNeighb[i] <- (  min(abs(ss - drc[i]), 360 - abs(ss - drc[i])) < h)
     }
     X <- Data[whichNeighb]  # X local
     theta_loc <- drc[whichNeighb]
     nloc <- length(X)

     df_local <- data.frame(theta_loc=theta_loc, X=X)

     switch (EVIestimator,
             ML = {res <- AutoChoice_k_circ(df_local=df_local, EVIestimator="ML",
                                            ss=ss, useKernel=useKernel, b=b, concent=concent)},
             Mom = {res <- AutoChoice_k_circ(df_local=df_local, EVIestimator="Mom",
                                             ss=ss, useKernel=useKernel, b=b, concent=concent)  }
     )

     local_k_opt <- res$k_opt
     u_opt <- sort(df_local$X)[nloc-res$k_opt]

     allRES[[s_i]] <- list(local_k_opt=local_k_opt,
                           u_opt=u_opt)

   }

  close(pb)
  stopCluster(cl)

  local_k <- rep(NA, length(thetaGrid))
  u <- rep(NA, length(thetaGrid))
  for(s_i in 1:length(thetaGrid)){
    local_k[s_i] <- resultsParallel[[s_i]]$local_k_opt
    u[s_i] <- resultsParallel[[s_i]]$u_opt
  }

  dir <- circular(thetaGrid,units="degrees")

  fit <- kern.reg.circ.lin(dir, u, t=dir, method = "NW", bw=bw)
  u_smooth <- fit$y

  # recalculate k_local
  local_k_ <- rep(NA, length(thetaGrid))
  for(s_i in 1:length(thetaGrid)){
    ss <- thetaGrid[s_i]

    whichNeighb <- rep(NA, n)
    for(i in 1:n){
      whichNeighb[i] <- (  min(abs(ss - drc[i]), 360 - abs(ss - drc[i])) < h)
    }
    X <- Data[whichNeighb]
    local_k_[s_i] <- sum(X>u_smooth[s_i])

  }

  u_per_obs <- u_smooth[drc]


return(list(local_k_=local_k_, thr=u_smooth, thr_per_obs=u_per_obs))

}
