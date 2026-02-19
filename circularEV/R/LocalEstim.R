

#' Local bootstrap estimation of EVI, scale and T-year levels
#'
#' @param thr Threshold values along thetaGrid
#' @param nBoot Number of bootstrap resamples. Default to 100.
#' @param movThr Logical. If TRUE (default), moving threshold within the window
#'   used.
#' @param TTs T-year levels. For example, TTs = c(100, 10000).
#' @param timeRange Time range of the sample
#'
#' @inheritParams ThrSelection
#'
#' @return List including bootstrap estimates of EVI, scale and T-year levels.
#' @details See Konzen, E., Neves, C., and Jonathan, P. (2021). Modeling nonstationary extremes of storm severity: Comparing parametric and semiparametric inference. Environmetrics, 32(4), e2667.
#' @export
#'
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
#' data(thresholdExampleMom) # loads threshold example
#' thrResultMom <- thresholdExampleMom
#'
#' \donttest{
#'
#' h <- 60
#' useKernel <- TRUE
#' concent <- 10
#' movThr <- TRUE
#' nBoot <- 30
#' set.seed(1234)
#' output <- LocalEstim(Data=Data, drc=drc, thr=thrResultMom,
#'                      thetaGrid=thetaVec, nBoot=nBoot, EVIestimator="Mom", h=h,
#'                      useKernel=useKernel, concent=concent, movThr=movThr,
#'                      TTs=c(100, 10000), timeRange=timeRange)
#'
#' RLBoot <- output$RLBoot
#'
#' PlotParamEstim(bootEstimates=output$xiBoot, thetaGrid=thetaVec, ylab=bquote(hat(xi)),
#'                alpha=0.05, ylim=NULL, cex.axis=15, cex.lab=2, thrWidth=2)
#'
#' PlotParamEstim(bootEstimates=output$sigBoot, thetaGrid=thetaVec, ylab=bquote(hat(sigma)),
#'                alpha=0.05, ylim=NULL, cex.axis=15, cex.lab=2, thrWidth=2)
#'
#' # 100-year level
#' PlotRL(RLBootList=RLBoot, thetaGrid=thetaVec, Data=Data, drc=drc,
#'        TTs=c(100, 10000), whichPlot=1, alpha=0.05, ylim=NULL,
#'        pointSize=1, cex.axis=15, cex.lab=2, thrWidth=2)
#'
#' PolarPlotRL(RLBootList=RLBoot, thetaGrid=thetaVec, Data=Data, drc=drc,
#'             TTs=c(100, 10000), whichPlot=1, alpha=0.05, ylim=NULL,
#'             pointSize=4, fontSize=12, lineWidth=2)
#'
#' }
#'
#' ## See examples in vignette:
#' # vignette("localMethods", package = "circularEV")
LocalEstim <- function(Data, drc, thr=NULL,
                       thetaGrid, nBoot=100, EVIestimator="Mom", h=30,
                       useKernel=TRUE, concent=10, movThr=TRUE, TTs=NULL, timeRange=NULL){

  if(is.null(thr)){
    stop("Threshold 'thr' must be provided")
  }

  if(!all(drc%in%thetaGrid)){
    stop("thetaGrid must include all observed directions (drc)")
  }

  if(length(Data)!=length(drc)){
    stop("excesses and drc must have same length")
  }

  n <- length(Data)

  nThetas <- length(thetaGrid)
  xiBoot <- matrix(NA, nThetas, nBoot)
  sigBoot <- matrix(NA, nThetas, nBoot)
  if(!is.null(TTs)){
    RLBootList <- vector("list", length = length(TTs))
  }

  for(iboot in 1:nBoot){

    xi <- rep(NA, length(thetaGrid))
    sig <- rep(NA, length(thetaGrid))
    if(!is.null(TTs)){
      RLmat <- matrix(NA, length(thetaGrid), length(TTs))
    }

    for(s_i in seq_along(thetaGrid)){

      ss <- thetaGrid[s_i]

      whichNeighb <- rep(NA, n)
      for(i in 1:n){
        whichNeighb[i] <- (  min(abs(ss - drc[i]), 360 - abs(ss - drc[i])) <= h)
      }
      X <- Data[whichNeighb]  # X local
      theta_loc <- drc[whichNeighb]
      nloc <- length(X)

      df_local <- data.frame(theta_loc=theta_loc, X=X)

      localThr <- thr[s_i]

      ########################################

      if(!useKernel){

        if(movThr){
          df_local$u <- thr[df_local$theta_loc] # moving threshold within window
          whichExceedBool <- (df_local$X - df_local$u) > 0
          k <- sum(whichExceedBool)
          whichExceed <- which(whichExceedBool)
          iBootIdx <- sample(x = whichExceed, size = k, replace = TRUE)
          excesses <- df_local$X[iBootIdx] - df_local$u[iBootIdx]

          m2log <- mean((log(df_local$X[iBootIdx]) - log(df_local$u[iBootIdx]))^2)
          H <- mean((log(df_local$X[iBootIdx]) - log(df_local$u[iBootIdx])))

          switch (EVIestimator,
                  ML = {res <- MLEstMovThr(excesses=excesses, n=nloc, localThr=localThr,
                                           asymptotic=FALSE, TTs=TTs, timeRange=timeRange)},
                  Mom = {res <- MomEstMovThr(H=H, m2log=m2log, k=k, n=nloc, localThr=localThr,
                                             asymptotic=FALSE, TTs=TTs, timeRange=timeRange)  }
          )
        }else{
          k <- sum(X>localThr)
          if(!(k<nloc)){k <- nloc-1}
          Xexceed <- X[(nloc-k+1):nloc]
          iBootIdx <- sample(x = 1:k, size = k, replace = TRUE)
          Xexceedboot <- Xexceed[iBootIdx]
          Xboot <- sort(c(X[1:(nloc-k)], Xexceedboot))

          switch (EVIestimator,
                  ML = {res <- MLEst(X=Xboot, k=k, asymptotic=FALSE, TTs=TTs, timeRange=timeRange)  },
                  Mom = {res <- MomEst(X=Xboot, k=k, asymptotic=FALSE, TTs=TTs, timeRange=timeRange)  }
          )
        }

      }else{ # useKernel is TRUE

        if(movThr){
          df_local$u <- thr[df_local$theta_loc] # moving threshold within window
          whichExceedBool <- (df_local$X - df_local$u) > 0
          k <- sum(whichExceedBool)
          if(!(k<nloc)){k <- nloc-1}
          whichExceed <- which(whichExceedBool)
          iBootIdx <- sample(x = whichExceed, size = k, replace = TRUE)

          excesses <- df_local$X[iBootIdx] - df_local$u[iBootIdx]

          Q1 <- (log(df_local$X[iBootIdx]) - log(df_local$u[iBootIdx]))
          Q2 <- (log(df_local$X[iBootIdx]) - log(df_local$u[iBootIdx]))^2

          ss_i <- df_local$theta_loc[iBootIdx]
          L <- myKernel(x = ss_i, x0 = ss, concent = concent)[1,]

          H <- sum(L*Q1)
          M2 <- sum(L*Q2)

          switch (EVIestimator,
                  ML = {res <- MLEstKernelMovThr(excesses=excesses, L=L, k=k, n=nloc,
                                                 localThr=localThr, asymptotic=FALSE,
                                                 restrict=FALSE, TTs=TTs, timeRange=timeRange) },
                  Mom = {res <- MomEstKernelMovThr(H=H, M2=M2, k=k, n=nloc,
                                                   localThr=localThr, asymptotic=FALSE,
                                                   TTs=TTs, timeRange=timeRange)  }
          )
        }else{

          k <- sum(X>localThr)
          if(!(k<nloc)){k <- nloc-1}

          ordX <- order(df_local$X, decreasing = FALSE)
          df_local_Xsorted <- df_local[ordX,]
          X <- df_local_Xsorted$X
          nloc <- length(X)

          excesses <- (X-X[nloc-k])[nloc:(nloc-k+1)]

          log_u <- log(localThr)
          Q1 <- ( log(df_local_Xsorted$X[nloc:(nloc-k+1)]) - log_u)^1
          Q2 <- ( log(df_local_Xsorted$X[nloc:(nloc-k+1)]) - log_u)^2

          ss_i <- df_local_Xsorted$theta_loc[nloc:(nloc-k+1)]
          L <- myKernel(x = ss_i, x0 = ss, concent = concent)[1,]

          H <- sum(L*Q1)
          M2 <- sum(L*Q2)

          switch (EVIestimator,
                  ML = {res <- MLEstKernel(excesses=excesses, L=L, k=k,
                                           n=nloc, localThr=localThr, asymptotic=FALSE,
                                           TTs=TTs, timeRange=timeRange)  },
                  Mom = {res <- MomEstKernel(H=H, M2=M2, k=k, n=nloc,
                                             localThr=localThr, asymptotic=FALSE,
                                             TTs=TTs, timeRange=timeRange)  }
          )
        }

      }

      xi[s_i] <- res$xi
      sig[s_i] <- res$scale
      RLmat[s_i, ] <- res$RL

    }

    xiBoot[,iboot] <- xi
    sigBoot[,iboot] <- sig
    RLBootList[[iboot]] <- RLmat

    cat(paste0("iboot=", iboot, " out of ", nBoot, "\n"))
  }


  return(list(xiBoot=xiBoot, sigBoot=sigBoot, RLBoot=RLBootList))
}

