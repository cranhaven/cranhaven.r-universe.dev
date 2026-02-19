#' Spline ML fitting
#'
#' @param excesses Excesses data
#' @param thetaVec Grid values at which the threshold will be evaluated
#' @param nBoot Number of bootstrap resamples
#' @param numIntKnots Number of internal knots
#' @param knotsType Position of knots. Default to "eqSpaced". Otherwise, the
#' knots will be placed at the quantiles of observed directions.
#' @param lambda Penalty parameter values for lambda
#' @param kappa Penalty parameter values for kappa
#' @param nCandidatesInit Number of initial parameter vectors.
#' Optimisation will start with the best.
#'
#' @inheritParams ThrSelection
#'
#' @return List of bootstrap estimates of shape and scale, and optimal values of lambda and kappa.
#' @details See Konzen, E., Neves, C., and Jonathan, P. (2021). Modeling nonstationary extremes of storm severity: Comparing parametric and semiparametric inference. Environmetrics, 32(4), e2667.
#' @export
#' @importFrom parallel makePSOCKcluster
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @import foreach
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
#' data(thresholdExampleML) # loads threshold example
#' thrResultML <- thresholdExampleML
#'
#' lambda <- 100
#' kappa <- 40
#'
#' thrPerObs <- thrResultML[drc]
#' excess <- Data - thrPerObs
#' drcExcess <- drc[excess>0]
#' excess <- excess[excess>0]
#'
#' \donttest{
#'
#' splineFit <- SplineML(excesses = excess, drc = drcExcess, nBoot = 30,
#'                       numIntKnots = 16, lambda=lambda, kappa=kappa, numCores=2)
#'
#' xiBoot <- splineFit$xi
#' sigBoot <- splineFit$sig
#'
#' PlotParamEstim(bootEstimates=xiBoot, thetaGrid=0:360, ylab=bquote(hat(xi)),
#'                alpha=0.05, ylim=NULL, cex.axis=15, cex.lab=2, thrWidth=2)
#'
#' PlotParamEstim(bootEstimates=sigBoot, thetaGrid=0:360, ylab=bquote(hat(sigma)),
#'                alpha=0.05, ylim=NULL, cex.axis=15, cex.lab=2, thrWidth=2)
#'
#' h <- 60 # needed for calculating local probability of exceedances
#' RLBoot <- CalcRLsplineML(Data=Data, drc=drc, xiBoot=xiBoot, sigBoot=sigBoot, h=h,
#'                          TTs=c(100, 10000), thetaGrid=thetaVec,
#'                          timeRange=timeRange, thr=thrResultML)
#'
#' # 100-year level
#' PlotRL(RLBootList=RLBoot, thetaGrid=thetaVec, Data=Data, drc=drc,
#'        TTs=c(100, 10000), whichPlot=1, alpha=0.05, ylim=NULL,
#'        pointSize=1, cex.axis=15, cex.lab=2, thrWidth=2)
#'
#' PolarPlotRL(RLBootList=RLBoot, thetaGrid=thetaVec, Data=Data, drc=drc,
#'             TTs=c(100, 10000), whichPlot=1, alpha=0.05, ylim=c(0, 25),
#'             pointSize=4, fontSize=12, lineWidth=2)
#'
#' }
#'
#' ## See also examples in vignette:
#' # vignette("splineML", package = "circularEV")
SplineML <- function(excesses, drc, thetaVec=0:360, nBoot=100,
                     numIntKnots=10, knotsType="eqSpaced",
                     lambda=seq(0,2, by=0.5), kappa=seq(0,2, by=0.5),
                     nCandidatesInit=1000, numCores=2){


  if(numCores > parallel::detectCores() - 2){
    numCores <- parallel::detectCores() - 2
  }
  cat(paste0(numCores, " CPU core(s) will be used \n"))

  if(length(excesses)!=length(drc)){
    stop("excesses and drc must have same length")
  }

  lambdaskappas_df <- expand.grid(lambda=lambda, kappa=kappa)

  xiBootMatList <- list()
  sigBootMatList <- list()
  predloglikVec <- rep(NA, nrow(lambdaskappas_df))

  for(i_pen in 1:nrow(lambdaskappas_df)){

    lambda <- lambdaskappas_df$lambda[i_pen]
    kappa <- lambdaskappas_df$kappa[i_pen]

    xiBootMat <- matrix(NA, length(thetaVec), nBoot)
    sigBootMat <- matrix(NA, length(thetaVec), nBoot)
    predloglik <- rep(NA, nBoot)
    loglikVec <- rep(NA, nBoot)

    ####   Placement of knots --------------------------------------------
    if(knotsType=="eqSpaced"){
      knots <- stats::quantile(thetaVec, probs = seq(0,1,length.out = 2+numIntKnots))
    }else{
      knots <- stats::quantile(as.numeric(drc), probs = seq(0,1,length.out = 2+numIntKnots))
    }
    knots[1] <- thetaVec[1]
    knots[length(knots)] <- thetaVec[length(thetaVec)]
    knots <- as.numeric(knots)

    # ### Cyclic B-splines --------------------------------------------------------------------
    bspl <- mgcv::cSplineDes(x = thetaVec, knots = knots, ord = 4, derivs = 0)
    num_alphas <- ncol(bspl) # 1 + numIntKnots

    ### Penalised Cyclic B-splines --------------------------------------------------------------------
    numPars <- 2                         # number of varying parameters: xi and sigma

    num_coeffs_omegas <- num_alphas*numPars  # number of basis functions per varying parameter
    n_hp <- num_coeffs_omegas                # num_alphas # num omegas + num betas for logsig2

    where_xi <- 1:num_alphas             # index of coefficients of xi in hp vector
    where_sig <- (num_alphas+1):n_hp     # index of coefficients of sig in hp vector


    parInit <- c(rep(-0.1, num_alphas), rep(log(2), num_alphas))

    ## ----------------------------------------------------------------------------------------

    cl <- makePSOCKcluster(numCores, outfile="")
    registerDoParallel(cl)
    pb <- txtProgressBar(title = "Progress",
                         min = 0, max = nBoot, style = 3)


    resultsParallel <- foreach::foreach(iboot=1:nBoot,
                                        .multicombine = FALSE,
                                        .export = c("negloglik_VaryingParams",
                                                    "Dnegloglik_VaryingParams",
                                                    "GPDcond_per_i_iFUN",
                                                    "negloglik_per_iFUN",
                                                    "negloglik_TestSet")) %dopar% {

      setTxtProgressBar(pb, iboot)

      # for(iboot in 1:nBoot){

      set.seed(iboot)
      n_excess <- length(excesses)
      iBootIdx <- sort(sample(x = 1:n_excess, size = n_excess, replace = TRUE))
      Not_iBootIdx <- which(!(1:n_excess)%in%iBootIdx)

      excess_iboot <- excesses[iBootIdx]
      angle_exceed_iboot <- drc[iBootIdx]

      excess_Notiboot <- excesses[Not_iBootIdx]
      angle_exceed_Notiboot <- drc[Not_iBootIdx]


      fit <- stats::optim(par = parInit, fn = negloglik_VaryingParams,
                   y=excess_iboot, angle=angle_exceed_iboot,
                   yfull=excesses, angfull=drc, bspl=bspl,
                   where_xi=where_xi, where_sig=where_sig,
                   gr = Dnegloglik_VaryingParams,
                   method="BFGS",
                   lambda=lambda, kappa=kappa)
      loglik <- fit$value

      converg <- fit$convergence
      hp <- fit$par

      xi <- c(bspl%*%hp[where_xi])
      sig <- exp(bspl%*%hp[where_sig])

      # xiBootMat[,iboot] <- xi               # line not used if parallellised
      # sigBootMat[,iboot] <- sig             # line not used if parallellised

      predloglik <- - negloglik_TestSet(hp=hp, ydata=excess_Notiboot,
                                        angle_exceed=angle_exceed_Notiboot,
                                        bspl=bspl, where_xi=where_xi,
                                        where_sig=where_sig)

      # cat(paste0("iboot=", iboot, " out of ", nBoot, "\n"))
      #   } # ends bootstrap

      return(list(xi=xi, sig=sig, loglik=loglik, predloglik=predloglik, converg=converg))
    }   # ends parallelised bootstrap

    close(pb)
    stopCluster(cl)

    for(iboot in 1:nBoot){
      xiBootMat[,iboot] <- resultsParallel[[iboot]]$xi
      sigBootMat[,iboot] <- resultsParallel[[iboot]]$sig
      predloglik[iboot] <- resultsParallel[[iboot]]$predloglik
      loglikVec[iboot] <- resultsParallel[[iboot]]$loglik
    }

    xiBootMatList[[i_pen]] <- xiBootMat
    sigBootMatList[[i_pen]] <- sigBootMat
    predloglikVec[i_pen] <- mean(predloglik)

    cat(paste0("i_pen = ", i_pen, " out of ", nrow(lambdaskappas_df), " done.", "\n"))


  }

  lambdaskappas_df$predloglik <- predloglikVec

  i_pen_opt <- which.max(lambdaskappas_df$predloglik)

  lambda_opt <- lambdaskappas_df[i_pen_opt,"lambda"]
  kappa_opt <- lambdaskappas_df[i_pen_opt,"kappa"]

  xiBootMat_opt <- xiBootMatList[[i_pen_opt]]
  sigBootMat_opt <- sigBootMatList[[i_pen_opt]]

  return(list(xi=xiBootMat_opt, sig=sigBootMat_opt, lambda=lambda_opt, kappa=kappa_opt))

}
