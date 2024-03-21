readMPMData  <-  function(t1Files  = NULL,
                          pdFiles  = NULL,
                          mtFiles  = NULL,
                          maskFile = NULL,
                          # sdim     = NULL,
                          TR       = NULL,
                          TE       = NULL,
                          FA       = NULL,
                          wghts    = NULL,
                          verbose  = TRUE) {

  ## we need at least T1w and PDw files
  if (is.null(t1Files)) stop("vector of T1 files required")
  # if (is.null(pdFiles)) stop("vector of PD files required")
  ## TODO: test whether there are enough files for the model?

  sdim <- dim(readNIfTI(t1Files[1], read_data = FALSE))

  # if (is.null(sdim)) stop("need spatial dimensionality of the data")
  # if (!is.numeric(sdim) | length(sdim) != 3) stop("need exactly three numbers for spatial dimensions")

  ## select the model according to the existence of MTw files
  model <- if (is.null(mtFiles)) {
    if (is.null(pdFiles)) {
      0L
    } else {
      1L # the simple model without MT inclusion
    }
  } else {
    2L # the model including MT
  }
  # first read or set the mask array
  ## TODO: set mask TRUE also if maskFile does not exist or cannot be read by readNIFTI()
  if (is.null(maskFile)) {
    if (verbose) cat("found no mask file, setting mask TRUE everywhere")
    mask <- array(TRUE, sdim)
  } else {
    if (verbose) cat("reading mask file ... ")
    mask <- as.logical(readNIfTI(maskFile, reorient = FALSE))
    dim(mask) <- sdim
    if (verbose) cat("done\n")
  }
  nvoxel <- sum(mask)

  ## count the number of data volumes (if (is.null(mtFiles)) length(mtFiles) == 0)
  nFiles <- length(t1Files) + length(mtFiles) + length(pdFiles)
  if(is.null(wghts)) wghts <- rep(1, nFiles)
  # the array for the data itself, will only contain data for voxel within mask
  ddata <- array(0, c(nFiles, nvoxel))

  ## for each files we have a TR, TE, and flip angle (FA)
  if (is.null(TR) || is.null(TE) || is.null(FA)) {
    TR <- TE <- FA <- numeric(nFiles)
    readParameterFlag <- TRUE
  } else {
    if (length(TR) != nFiles) stop("not enough TR value, need as many as file!")
    if (length(TE) != nFiles) stop("not enough TE value, need as many as file!")
    if (length(FA) != nFiles) stop("not enough FA value, need as many as file!")
    readParameterFlag <- FALSE
  }

  ## ... now we read all data volumes and extract the TR, TE, and FA values for each ...
  ii <- 1
  ## ... for all T1 volumes ...
  if (verbose) cat("reading T1 files\n")
  if (verbose) pb <- txtProgressBar(min = 0, max = length(t1Files), style = 3)
  for (i in 1:length(t1Files)) {
    ds <- readNIfTI(t1Files[i], reorient = FALSE)
    ddata[ii, ] <- ds[mask]
    if(readParameterFlag) {
      ## IMPORTANT: This is special to Siawoosh data
      res <- stringr::str_match_all(ds@descrip, "([[:alpha:]]{2})=([.0123456789]+)([[:alpha:]]{2,})")
      for (nn in 1:dim(res[[1]])[1]) {
        if (res[[1]][nn, 2] == "TR") TR[ii] <- as.numeric(res[[1]][nn, 3])
        if (res[[1]][nn, 2] == "TE") TE[ii] <- as.numeric(res[[1]][nn, 3])
        if (res[[1]][nn, 2] == "FA") FA[ii] <- as.numeric(res[[1]][nn, 3])
      }
    }
    ii <- ii + 1
    if (verbose) setTxtProgressBar(pb, i)
  }
  if (verbose) close(pb)
  ## ... for all MT volumes ...
  if (model == 2) {
    if (verbose) cat("reading MT files\n")
    if (verbose) pb <- txtProgressBar(min = 0, max = length(mtFiles), style = 3)
    for (i in 1:length(mtFiles)) {
      ds <- readNIfTI(mtFiles[i], reorient = FALSE)
      ddata[ii, ] <- ds[mask]
      if(readParameterFlag) {
        ## IMPORTANT: This is special to Siawoosh data
        res <- stringr::str_match_all(ds@descrip, "([[:alpha:]]{2})=([.0123456789]+)([[:alpha:]]{2,})")
        for (nn in 1:dim(res[[1]])[1]) {
          if (res[[1]][nn, 2] == "TR") TR[ii] <- as.numeric(res[[1]][nn, 3])
          if (res[[1]][nn, 2] == "TE") TE[ii] <- as.numeric(res[[1]][nn, 3])
          if (res[[1]][nn, 2] == "FA") FA[ii] <- as.numeric(res[[1]][nn, 3])
        }
      }
      ii <- ii + 1
      if (verbose) setTxtProgressBar(pb, i)
    }
    if (verbose) close(pb)
  }
  ## .. and for all PD volumes ...
  if ((model == 2) || (model == 1)) {
    if (verbose) cat("reading PD files\n")
    if (verbose) pb <- txtProgressBar(min = 0, max = length(pdFiles), style = 3)
    for (i in 1:length(pdFiles)) {
      ds <- readNIfTI(pdFiles[i], reorient = FALSE)
      ddata[ii, ] <- ds[mask]
      if(readParameterFlag) {
        ## IMPORTANT: This is special to Siawoosh data
        res <- stringr::str_match_all(ds@descrip, "([[:alpha:]]{2})=([.0123456789]+)([[:alpha:]]{2,})")
        for (nn in 1:dim(res[[1]])[1]) {
          if (res[[1]][nn, 2] == "TR") TR[ii] <- as.numeric(res[[1]][nn, 3])
          if (res[[1]][nn, 2] == "TE") TE[ii] <- as.numeric(res[[1]][nn, 3])
          if (res[[1]][nn, 2] == "FA") FA[ii] <- as.numeric(res[[1]][nn, 3])
        }
      }
      ii <- ii + 1
      if (verbose) setTxtProgressBar(pb, i)
    }
    if (verbose) close(pb)
  }
  ## ... done!

  ## exclude all voxel from mask with all zeros for a modality
  if (verbose) cat("Searching for voxel with zeros only in any modality ...")
  nT1 <- length(t1Files)
  nMT <- length(mtFiles)
  nPD <- length(pdFiles)
  zerovoxel <- apply(ddata[1:nT1,] <= 0, 2, all)
  if(model>0) zerovoxel <- zerovoxel|apply(ddata[nT1+nMT+1:nPD,] <= 0, 2, all)
  if(model==2) zerovoxel <- zerovoxel|apply(ddata[nT1+1:nMT,] <= 0, 2, all)
  mask[mask] <- !zerovoxel ## exclude zerovoxel from mask
  ddata <- ddata[, !zerovoxel] ## remove zerovoxel from ddata

  obj <- list(ddata = ddata,
              sdim = sdim,
              nFiles = nFiles,
              t1Files = t1Files,
              pdFiles = pdFiles,
              mtFiles = mtFiles,
              model = model,
              maskFile = maskFile,
              mask = mask,
              TR = TR,
              TE = TE,
              FA = FA,
              weights = wghts)
  class(obj) <- "MPMData"
  invisible(obj)
}

estimateSigma <- function(magnitude,phase,mask,kstar=20,kmin=8,hsig=5,lambda=12,verbose=TRUE){
  ## kmin = 10 corresponds to an initial bandwidth of 1.47 giving positive weight to direct neighbors and
  ## 2D diagonal neigbors
  args <- sys.call(-1)
  sdim <- dim(mask)
  if(!is.numeric(magnitude)){
    if (verbose) cat("reading Magnitude file ... ")
    R <- readNIfTI(magnitude, reorient = FALSE)
  } else {
    R <- magnitude
  }
  if(!is.numeric(phase)){
    if (verbose) cat("reading Phase file ... ")
    Ph <- readNIfTI(phase, reorient = FALSE)
  } else {
    Ph <- phase
  }
  z <- aws::estimateSigmaCompl(R, Ph, ,mask, kstar=kstar, kmin=kmin, hsig=hsig,
    lambda=lambda, verbose=verbose)
  class(z) <- "sigmaEstSENSE"
  z
}

medianFilterSigma <- function(obj,hsig=10,mask=NULL){
  if(inherits(obj, "sigmaEstSENSE")){
    sigma2 <- obj$sigmal^2
    mask <- obj$mask
  } else {
    sigma2 <- obj^2
  }
  sigma2hat <- aws::medianFilter3D(sigma2, h=hsig, mask=mask)/0.6931
  if(inherits(obj, "sigmaEstSENSE")){
    obj$sigma <- sqrt(sigma2hat)
    obj$hsig <- hsig
  } else {
    obj <- sqrt(sigma2hat)
    dim(obj) <- dim(sigma2hat)
  }
  obj
}

validate_MPMData = function(mpmdata) {
  need_names = c("model", "nFiles", "t1Files",
                 "maskfile", "mask",
                 "ddata", "TE", "TR", "FA",
                 "sdim")
  n = names(mpmdata)
  check = need_names %in% n
  is_null = sapply(need_names, function(x) {
    is.null(mpmdata[[x]])
  })
  if (!all(check) | any(is_null)) {
    bad = need_names[!check | is_null]
    bad = paste(bad, sep = ", ")
    stop(paste0("These slots are not available: ", bad))
  }
}

estimateESTATICS <- function (mpmdata,
                              TEScale = 100,
                              dataScale = 1000,
                              method = c("NLR", "QL"),
                              sigma = NULL,
                              L = 1,
                              maxR2star=50,
                              varest = c("RSS","data"),
                              verbose = TRUE) {

  # validate_MPMData(mpmdata)
  mask <- mpmdata$mask
  sdim <- mpmdata$sdim
  nvoxel <- sum(mask)
  method <- method[1]
  varest <- varest[1]
  ## create the design matrix of the model
  nT1 <- length(mpmdata$t1Files)
  nMT <- length(mpmdata$mtFiles)
  nPD <- length(mpmdata$pdFiles)
  if (mpmdata$model == 2) {
    xmat <- matrix(0, mpmdata$nFiles, 4)
    xmat[1:nT1, 1] <- 1
    xmat[(nT1 + 1):(nT1 + nMT), 2] <- 1
    xmat[(nT1 + nMT + 1):mpmdata$nFiles, 3] <- 1
    xmat[, 4] <- mpmdata$TE/TEScale
    ## ... for our model in estatics3() ...
    ## S_{T1} = par[1] * exp(- par[4] * TE)
    ## S_{MT} = par[2] * exp(- par[4] * TE)
    ## S_{PD} = par[3] * exp(- par[4] * TE)
  } else if (mpmdata$model == 1) {
    xmat <- matrix(0, mpmdata$nFiles, 3)
    xmat[1:nT1, 1] <- 1
    xmat[(nT1 + 1):mpmdata$nFiles, 2] <- 1
    xmat[, 3] <- mpmdata$TE / TEScale
    ## ... for our model in estatics2() ...
    ## S_{T1} = par[1] * exp(- par[3] * TE)
    ## S_{PD} = par[2] * exp(- par[3] * TE)
  } else {
    xmat <- matrix(0, mpmdata$nFiles, 2)
    xmat[1:nT1, 1] <- 1
    xmat[, 2] <- mpmdata$TE / TEScale
    ## ... for our model in estatics1() ...
    ## S_{T1} = par[1] * exp(- par[2] * TE)
  }
  if (verbose) {
    cat("Design of the model:\n")
    print(xmat)
  }

  if (verbose) cat(" done\n")
  modelp1 <- mpmdata$model+1
  wghts <- mpmdata$wghts
  if(is.null(wghts)) wghts <- rep(1, mpmdata$nFiles)
  if(varest=="data"){
     if(verbose) cat("estimating variance maps from data\n")
     ind <- switch(modelp1,1,c(1,1+nT1),c(1,1+nT1,1+nT1+nMT))
     sind <- switch(modelp1,rep(1,nT1), c(rep(1,nT1),rep(2,nPD)),
                                c(rep(1,nT1), rep(2,nMT), rep(3,nPD)))
     ddata <- extract(mpmdata,"ddata")[ind,,,,drop=FALSE]
     shat <- ddata[ind,,,]
     for( i in 1:modelp1){
        shat[i,,,] <- aws::awsLocalSigma(ddata[ind[i],,,], steps=16,
            mask=extract(mpmdata,"mask"), ncoils=L, hsig=2.5,
            lambda=6,family="Gauss")$sigma
     }
     dim(shat) <- c(modelp1,prod(sdim))
     shat <- shat[,mask]
     shat[shat==0] <- quantile(shat,.8)
     if(is.null(sigma)) sigma <- median(shat)
     shat <- shat/dataScale
   } else {
      shat <- array(0,c(modelp1,nvoxel))
      sind <- NULL
    }
  ## obbtain initial estimates from linearized model
  thetas <- initth(mpmdata, TEScale, dataScale)
  ## now only contains estimates for voxel in mask
  ## set lower and upper vales for parameters
  lower <- pmax(.001,apply(thetas,1,min))
  upper <- pmin(c(rep(65532/dataScale,modelp1),50),apply(thetas,1,max))
## prepare the standard deviation array in case of the quasi-likelihood estimation (QL)
  if (method == "QL") {
    sigma <- sigma/dataScale
    CLarray <- sigma * sqrt(pi/2) * gamma(L + 0.5)/gamma(L)/gamma(1.5)
    if (length(sigma) == 1) {
      homsigma <- TRUE
      sig <- sigma
      CL <- CLarray
    } else if (all(dim(sigma) == sdim)) {
      homsigma <- FALSE
      sigma <- sigma[mask]
      CLarray <- CLarray[mask]
    ## only need values within mask
    } else {
      stop("Dimension of argument sigma does not match the data")
    }
  } else {
  # not used
     sigma <- 1
     CLarray <- 1
  }

  ## create inde vectors for the data with different weighting (T1w, MTw, PDw)
  indT1 <- order(mpmdata$TE[as.logical(xmat[, 1])])[1]
  if (mpmdata$model == 2) {
    indMT <- order(mpmdata$TE[as.logical(xmat[, 2])])[1] + sum(xmat[, 1])
    indPD <- order(mpmdata$TE[as.logical(xmat[, 3])])[1] + sum(xmat[, 1]) + sum(xmat[, 2])
    npar <- 4
  } else if (mpmdata$model == 1) {
    indPD <- order(mpmdata$TE[as.logical(xmat[, 2])])[1] + sum(xmat[, 1])
    npar <- 3
  } else {
    npar <- 2
  }

  ## create necessary arrays
  isConv <- array(FALSE, nvoxel)
  isThresh <- array(FALSE, nvoxel)
  modelCoeff <- array(0, c(npar, nvoxel))
  invCov <- array(0, c(npar, npar, nvoxel))
  rsigma <- array(0, nvoxel)


  if (verbose){
     cat("Start estimation in", nvoxel, "voxel at", format(Sys.time()), "\n")
     if(setCores()==1) pb <- txtProgressBar(0, nvoxel, style = 3)
   }
if( setCores() >1){
        x <- array(0,c(mpmdata$nFiles+npar+modelp1+2,nvoxel))
        x[mpmdata$nFiles+1:npar,] <- thetas
        x[1:mpmdata$nFiles,] <- mpmdata$ddata/dataScale
        x[mpmdata$nFiles+npar+1:modelp1,] <- shat
        x[mpmdata$nFiles+npar+modelp1+1,] <- sigma
        x[mpmdata$nFiles+npar+modelp1+2,] <- CLarray
 #       ergs <- array(0, c(npar+npar*npar+2,nvoxel))
       ergs <- switch(mpmdata$model+1,plmatrix(x,pEstESTATICS0,method,varest,
                                     xmat,wghts,maxR2star,L,lower,upper,sind),
                   plmatrix(x,pEstESTATICS1,method,varest,
                                     xmat,wghts,maxR2star,L,lower,upper,sind),
                   plmatrix(x,pEstESTATICS2,method,varest,
                                     xmat,wghts,maxR2star,L,lower,upper,sind))
        isConv <- ergs[npar+npar*npar+1,]
        modelCoeff <- ergs[1:npar, ]
        invCov <- ergs[npar+1:(npar*npar), ]
        dim(invCov) <- c(npar,npar,nvoxel)
        rsigma <- ergs[npar+npar*npar+2,]

} else {
  for(xyz in 1:nvoxel){

          if (method == "QL") {
            if(!homsigma) {
              sig <- sigma[xyz]
              CL <- CLarray[xyz]
            }
          }

          ivec <- mpmdata$ddata[, xyz]/dataScale
          th <- thetas[, xyz]

          if (mpmdata$model == 2) {
            res <- if (method == "NLR") try(nls(ivec ~ estatics3(par, xmat),
                                                data = list(xmat = xmat),
                                                start = list(par = th),
                                                weights = wghts,
                                                control = list(maxiter = 200,
                                                               warnOnly = TRUE)))
            else try(nls(ivec ~ estatics3QL(par, xmat, CL, sig, L),
                         data = list(xmat = xmat,
                                     CL = CL,
                                     sig = sig,
                                     L = L),
                         start = list(par = th),
                         weights = wghts,
                         control = list(maxiter = 200,
                                        warnOnly = TRUE)))
            if (inherits(res, "try-error")){
# retry with port algorithm and bounds
              th <- pmin(upper,pmax(lower,th))
              res <- if (method == "NLR") try(nls(ivec ~ estatics3(par, xmat),
                                                  data = list(xmat = xmat),
                                                  start = list(par = th),
                                                  algorithm="port",
                                                  weights = wghts,
                                                  control = list(maxiter = 200,
                                                                 warnOnly = TRUE),
                                                  lower=lower, upper=upper))
              else try(nls(ivec ~ estatics3QL(par, xmat, CL, sig, L),
                           data = list(xmat = xmat,
                                       CL = CL,
                                       sig = sig,
                                       L = L),
                           start = list(par = th),
                           algorithm="port",
                           weights = wghts,
                           control = list(maxiter = 200,
                                          warnOnly = TRUE),
                           lower=lower, upper=upper))
            }
          } else if (mpmdata$model == 1) {
            res <- if (method == "NLR") try(nls(ivec ~ estatics2(par, xmat),
                                                data = list(xmat = xmat),
                                                start = list(par = th),
                                                weights = wghts,
                                                control = list(maxiter = 200,
                                                               warnOnly = TRUE)))
            else try(nls(ivec ~ estatics2QL(par, xmat, CL, sig, L),
                         data = list(xmat = xmat,
                                     CL = CL,
                                     sig = sig,
                                     L = L),
                         start = list(par = th),
                         weights = wghts,
                         control = list(maxiter = 200,
                                        warnOnly = TRUE)))
            if (inherits(res, "try-error")){
             # retry with port algorithm and bounds
                  th <- pmin(upper,pmax(lower,th))
                  res <- if (method == "NLR") try(nls(ivec ~ estatics2(par, xmat),
                                                      data = list(xmat = xmat),
                                                      start = list(par = th),
                                                      algorithm="port",
                                                      weights = wghts,
                                                      control = list(maxiter = 200,
                                                                     warnOnly = TRUE),
                                                      lower=lower, upper=upper))
                  else try(nls(ivec ~ estatics2QL(par, xmat, CL, sig, L),
                               data = list(xmat = xmat,
                                           CL = CL,
                                           sig = sig,
                                           L = L),
                               start = list(par = th),
                               algorithm="port",
                               weights = wghts,
                               control = list(maxiter = 200,
                                              warnOnly = TRUE),
                               lower=lower, upper=upper))
            }
          } else if (mpmdata$model == 0) {
            res <- if (method == "NLR") try(nls(ivec ~ estatics1(par, xmat),
                                                data = list(xmat = xmat),
                                                start = list(par = th),
                                                weights = wghts,
                                                control = list(maxiter = 200,
                                                               warnOnly = TRUE)))
            else try(nls(ivec ~ estatics1QL(par, xmat, CL, sig, L),
                         data = list(xmat = xmat,
                                     CL = CL,
                                     sig = sig,
                                     L = L),
                         start = list(par = th),
                         weights = wghts,
                         control = list(maxiter = 200,
                                        warnOnly = TRUE)))
             if (inherits(res, "try-error")){
# retry with port algorithm and bounds
                 th <- pmin(upper,pmax(lower,th))
                 res <- if (method == "NLR") try(nls(ivec ~ estatics1(par, xmat),
                                                     data = list(xmat = xmat),
                                                     start = list(par = th),
                                                     algorithm="port",
                                                     weights = wghts,
                                                     control = list(maxiter = 200,
                                                                    warnOnly = TRUE),
                                                     lower=lower, upper=upper))
                 else try(nls(ivec ~ estatics1QL(par, xmat, CL, sig, L),
                              data = list(xmat = xmat,
                                          CL = CL,
                                          sig = sig,
                                          L = L),
                              start = list(par = th),
                              algorithm = "port",
                              weights = wghts,
                              control = list(maxiter = 200,
                                             warnOnly = TRUE),
                              lower=lower, upper=upper))

              }
          }

          if (!inherits(res, "try-error")) {
            sres <- if(varest=="RSS") getnlspars(res) else
                 getnlspars2(res, shat[, xyz], sind )
            isConv[xyz] <- as.integer(res$convInfo$isConv)
            modelCoeff[, xyz] <- sres$coefficients
            if (sres$sigma != 0) {
              invCov[, , xyz] <- sres$invCov
              rsigma[xyz] <- sres$sigma
            }
          }

          if (inherits(res, "try-error") || coef(res)[npar] > maxR2star || coef(res)[npar] < 0) {

            ## fallback for not converged or R2star out of range
            sres <- if(varest=="RSS") linearizedESTATICS(ivec, xmat, maxR2star, wghts) else
                                        linearizedESTATICS2(ivec, xmat, maxR2star, shat[, xyz], sind ,wghts)
            ## thats already the solution for NLR if R2star is fixed
            isThresh[xyz] <- sres$invCov[npar, npar] == 0
            isConv[xyz] <- 255 ## partially linearized NLR model
            xmat0 <- sres$xmat
            th <- sres$theta
            modelCoeff[-npar, xyz] <- sres$theta
            modelCoeff[npar, xyz] <- sres$R2star
            if (sres$sigma2 != 0) {
              invCov[, , xyz] <- sres$invCov
              rsigma[xyz] <- sqrt(sres$sigma2)
            }

            if (method == "QL") {
              xmat0 <- sres$xmat
              # xmat0 containes design matrix for linear problem with fixed R2star
              # ony have nonlinearity from QL
              if (mpmdata$model == 2)
                res <- try(nls(ivec ~ estatics3QLfixedR2(par, xmat, CL, sig, L),
                               data = list(xmat = xmat0,
                                           CL = CL,
                                           sig = sig,
                                           L = L),
                               start = list(par = th),
                               algorithm ="port",
                               weights = wghts,
                               control = list(maxiter = 200,
                                              warnOnly = TRUE),
                               lower=lower[1:3], upper=upper[1:3]))
              else if (mpmdata$model == 1)
                res <- try(nls(ivec ~ estatics2QLfixedR2(par, xmat, CL, sig, L),
                               data = list(xmat = xmat0,
                                           CL = CL,
                                           sig = sig,
                                           L = L),
                               start = list(par = th),
                               algorithm ="port",
                               weights = wghts,
                               control = list(maxiter = 200,
                                              warnOnly = TRUE),
                               lower=lower[1:2], upper=upper[1:2]))
              else if (mpmdata$model == 0)
                res <- try(nls(ivec ~ estatics1QLfixedR2(par, xmat, CL, sig, L),
                               data = list(xmat = xmat0,
                                           CL = CL,
                                           sig = sig,
                                           L = L),
                               start = list(par = th),
                               algorithm ="port",
                               weights = wghts,
                               control = list(maxiter = 200,
                                              warnOnly = TRUE),
                               lower=lower[1], upper=upper[1]))
              if (!inherits(res,"try-error")) {
                isConv[xyz] <- as.integer(res$convInfo$isConv)
                sres <- getnlspars(res)
                modelCoeff[-npar, xyz] <- sres$coefficients
                if (sres$sigma != 0) {
                  invCovtmp <- sres$XtX
                  invCov[-npar, -npar, xyz] <- invCovtmp/sres$sigma^2
                  rsigma[xyz] <- sres$sigma
                }
              } else {
                posxyz <- ((1:prod(sdim))[mask])[xyz]
                mask[posxyz] <- FALSE
              }
            }
          }#fallback
#    if (verbose) if(xyz%/%10000*10000==xyz) cat("completed", xyz, "of", nvoxel, "voxel  time", format(Sys.time()), "\n")
if (verbose) if(xyz%/%1000*1000==xyz) setTxtProgressBar(pb, xyz)
  }#z
  }
  if (verbose){
    if(setCores()==1) close(pb)
    cat("Finished estimation", format(Sys.time()), "\n")
  }

  obj <- list(modelCoeff = modelCoeff,
              invCov = invCov,
              rsigma = rsigma,
              isThresh = isThresh,
              isConv = isConv,
              sdim = sdim,
              nFiles = mpmdata$nFiles,
              t1Files = mpmdata$t1Files,
              pdFiles = mpmdata$pdFiles,
              mtFiles = mpmdata$mtFiles,
              model = mpmdata$model,
              maskFile = mpmdata$maskFile,
              mask = mask,
              sigma = sigma,
              shat = shat, ## sigma estimated from
              L = L,
              TR = mpmdata$TR,
              TE = mpmdata$TE,
              FA = mpmdata$FA,
              TEScale = TEScale,
              dataScale = dataScale)

  class(obj) <- "ESTATICSModel"
  invisible(obj)
}

smoothESTATICS <- function(mpmESTATICSModel,
                           mpmData = NULL,
                           kstar = 16,
                           alpha = 0.025,
                           patchsize = 0,
                           mscbw = 5,
                           wghts = NULL,
                           verbose = TRUE) {#1
  ##
  ##  consistency checks
  ##
  nv <- mpmESTATICSModel$model+2
  mask <- mpmESTATICSModel$mask
  nvoxel <- sum(mask)
  sdim <- mpmESTATICSModel$sdim
  if(any(dim(mpmESTATICSModel$invCov)!=c(nv,nv,nvoxel))) stop("inconsistent invCov")
  if(any(dim(mask)!=sdim)) stop("inconsistent mask")
  if(any(dim(mpmESTATICSModel$modelCoeff)!=c(nv,nvoxel))) stop("inconsistent parameter length")
  if(!is.null(mpmData)){#2
      # allow for mpmData to be expanded or to only contain data within mask
      if(any(dim(mpmData)[-1]!=nvoxel)){#3
         if(all(dim(mpmData)[-1]==sdim)){#4
#  reduce mpmData to voxel within mask
            dim(mpmData) <- c(dim(mpmData)[1],prod(sdim))
            mpmData <- mpmData[,mask]
         } else stop("inconsistent mpmData")
      }#3
  }#2
  ## determine a suitable adaptation bandwidth
  patchsize <- pmax(0,pmin(2,as.integer(patchsize)))
  lambda <- 2 * nv * qf(1 - alpha, nv, mpmESTATICSModel$nFiles - nv)*
    switch(patchsize+1,1,2.77,3.46)
  #  factor 2 (analog to 2 sigma in KL) to have more common values for alpha
  #  factor for patchsizes adjusted using simulated data
  if(verbose) cat("using lambda=", lambda, " patchsize=", patchsize,"\n")
  invCov <- extract(mpmESTATICSModel,"invCov")
  if(mscbw>0){
     rsdx <- extract(mpmESTATICSModel,"rsigma")^2
     rsdhat <- medianFilter3D(rsdx,mscbw,mask)
     rsdhat[!mask] <- mean(rsdhat[mask])
     invCov <- sweep(invCov,3:5,rsdx/rsdhat,"*")
  }
  dim(invCov) <- c(nv,nv,prod(sdim))
  invCov <- invCov[,,mask]
  zobj <- vpawscov2(mpmESTATICSModel$modelCoeff,
                   kstar,
                   invCov,
                   mask,
                   lambda = lambda,
                   wghts = wghts,
                   patchsize = patchsize,
                   data = mpmData,
                   verbose = verbose)
  ## assign values
  obj <- list(modelCoeff = zobj$theta,
                 invCov = mpmESTATICSModel$invCov,
                 isConv = mpmESTATICSModel$isConv,
                 rsigma = mpmESTATICSModel$rsigma,
                 bi = zobj$bi,
                 smoothPar = c(zobj$lambda, zobj$hakt, alpha, patchsize, mscbw),
                 smoothedData = zobj$data,
                 sdim = mpmESTATICSModel$sdim,
                 nFiles = mpmESTATICSModel$nFiles,
                 t1Files = mpmESTATICSModel$t1Files,
                 pdFiles = mpmESTATICSModel$pdFiles,
                 mtFiles = mpmESTATICSModel$mtFiles,
                 model = mpmESTATICSModel$model,
                 maskFile = mpmESTATICSModel$maskFile,
                 mask = mask,
                 sigma = mpmESTATICSModel$sigma,
                 L = mpmESTATICSModel$L,
                 TR = mpmESTATICSModel$TR,
                 TE = mpmESTATICSModel$TE,
                 FA = mpmESTATICSModel$FA,
                 TEScale = mpmESTATICSModel$TEScale,
                 dataScale = mpmESTATICSModel$dataScale)
  class(obj) <- "sESTATICSModel"
  invisible(obj)

  ## END function smoothESTATICS()
}

calculateQI <- function(mpmESTATICSModel,
                        b1File = NULL,
                        TR2 = 0,
                        verbose = TRUE) {

  ## read B1 correction field
  if (mpmESTATICSModel$model==0){
     warning("No b1-field correction possible. Returning R2star and ST1 maps from ESTATICS model")
     obj <- list(R2star = mpmESTATICSModel$modelCoeff[2, ]/mpmESTATICSModel$TEScale,
                 ST1 = mpmESTATICSModel$modelCoeff[1, ]/mpmESTATICSModel$dataScale,
                 model = mpmESTATICSModel$model,
                 t1Files = mpmESTATICSModel$t1Files,
                 mtFiles = mpmESTATICSModel$mtFiles,
                 pdFiles = mpmESTATICSModel$pdFiles,
                 mask = mpmESTATICSModel$mask)
     class(obj) <- "qMaps"
     invisible(obj)
  }
  if (!is.null(b1File)) {
    if (verbose) cat("reading B1 correction file\n")# from\n", b1File, "\n")
    b1Map <- readNIfTI(b1File, reorient = FALSE)/100
    b1Map[b1Map < 0] <- 0
    if (any(dim(b1Map) != mpmESTATICSModel$sdim)) stop("dimension of B1 map does not match data dimension")
  } else {
    if (verbose) cat("no B1 correction\n")
    b1Map <- array(1, mpmESTATICSModel$sdim)
  }
  b1Map <- b1Map[mpmESTATICSModel$mask]
  ## get correct flip angles and TR times
  t1FA <- mpmESTATICSModel$FA[1]
  pdFA <- mpmESTATICSModel$FA[length(mpmESTATICSModel$t1Files) + length(mpmESTATICSModel$mtFiles) + 1]
  t1TR <- mpmESTATICSModel$TR[1]
  pdTR <- mpmESTATICSModel$TR[length(mpmESTATICSModel$t1Files) + length(mpmESTATICSModel$mtFiles) + 1]

  ## calculate E1
  if (verbose) cat("calculating R1 ... ")
  alphat1 <- b1Map * t1FA / 180 * pi
  alphapd <- b1Map * pdFA / 180 * pi
  SINalphat1 <- sin(alphat1)
  COSalphat1 <- cos(alphat1)
  SINalphapd <- sin(alphapd)
  COSalphapd <- cos(alphapd)
  rm(alphat1, alphapd)
  if (mpmESTATICSModel$model == 2) {
    enum <- mpmESTATICSModel$modelCoeff[1, ] - SINalphat1/SINalphapd * mpmESTATICSModel$modelCoeff[3, ]
    denom <- mpmESTATICSModel$modelCoeff[1, ] * COSalphat1 - SINalphat1/SINalphapd * mpmESTATICSModel$modelCoeff[3, ] * COSalphapd
  } else {
    enum <- mpmESTATICSModel$modelCoeff[1, ] - SINalphat1/SINalphapd * mpmESTATICSModel$modelCoeff[2, ]
    denom <- mpmESTATICSModel$modelCoeff[1, ] * COSalphat1 - SINalphat1/SINalphapd * mpmESTATICSModel$modelCoeff[2, ] * COSalphapd
  }
  E1 <- enum/denom
  rm(enum, denom, COSalphapd, SINalphapd)
  R1 <- -log(E1)/t1TR

  ### RF spoiling correction Preibisch and Deichmann MRM 61 (2009) 125-135
  ### These coefficients depend on the sequence!! See getPolynomsP2_ab and
  ### MTprot in VBQ
  P2_a = getPolynomsP2_ab(pdTR, t1TR, pdFA, t1FA, verbose)$P2_a
  P2_b = getPolynomsP2_ab(pdTR, t1TR, pdFA, t1FA, verbose)$P2_b
  R1 = R1 / ((P2_a[1]*b1Map^2 + P2_a[2]*b1Map + P2_a[3]) * R1 + (P2_b[1]*b1Map^2 + P2_b[2]*b1Map + P2_b[3]))
  E1 = exp(- R1 * t1TR)
  ### END spoiling correction

  if (verbose) cat("done\n")

  ## calculate PD
  if (verbose) cat("calculating PD ... ")
  enum <- (1 - COSalphat1 * E1) * mpmESTATICSModel$modelCoeff[1, ] * mpmESTATICSModel$dataScale
  denom <- SINalphat1 * (1 - E1)
  PD <- enum / denom
  rm(enum, denom, SINalphat1)
  if (verbose) cat("done\n")

  ## calculate delta
  if (mpmESTATICSModel$model == 2) {
    if (verbose) cat("calculating MT ... ")
    mtFA <- mpmESTATICSModel$FA[length(mpmESTATICSModel$t1Files) + 1]
    mtTR <- mpmESTATICSModel$TR[length(mpmESTATICSModel$t1Files) + 1]
    alphamt <- b1Map * mtFA / 180 * pi
    E1mt <- E1^(mtTR/t1TR)
    E2mt <- E1^(TR2/t1TR)
    enom <- mpmESTATICSModel$modelCoeff[2, ]  * mpmESTATICSModel$dataScale - (1 - E2mt) * sin(alphamt) * PD
    denom <- mpmESTATICSModel$modelCoeff[2, ]  * mpmESTATICSModel$dataScale * cos(alphamt) *E1mt + PD * (E2mt  - E1mt) * sin(alphamt)
    delta <- 1 - enom / denom
    rm(alphamt, enom, denom)

    ### correction for MT saturation pulse. see Helms ISMRM 23 (2015) 3360
    delta = 100 * delta * (1 - 0.4) / (1 - 0.4 * b1Map) / b1Map^2;

    if (verbose) cat("done\n")
  } else {
    delta <- NULL
  }
  R2star <- if (mpmESTATICSModel$model == 2) 1000 * mpmESTATICSModel$modelCoeff[4, ]/mpmESTATICSModel$TEScale else
                                             1000 * mpmESTATICSModel$modelCoeff[3, ]/mpmESTATICSModel$TEScale
  # set values outside the mask to NA as we have with the other qMaps due to denom=0
  obj <- list(b1Map = b1Map,
              R1 = R1 * 1000,
              R2star = R2star,
              PD = PD,
              MT = delta,
              sdim = mpmESTATICSModel$sdim,
              model = mpmESTATICSModel$model,
              t1Files = mpmESTATICSModel$t1Files,
              mtFiles = mpmESTATICSModel$mtFiles,
              pdFiles = mpmESTATICSModel$pdFiles,
              mask = mpmESTATICSModel$mask)
  class(obj) <- "qMaps"
  invisible(obj)
}

imageQI <- function(qi,
                    view = 1,
                    slice = 1) {

  mask <- switch(view,
                 qi$mask[slice, , ],
                 qi$mask[, slice, ],
                 qi$mask[, , slice])
  R2star <- extract(qi, "R2star")
  r2star <- switch(view,
                   R2star[slice, , ],
                   R2star[, slice, ],
                   R2star[, , slice])
  r2star[!mask] <- 0
  R1 <- extract(qi, "R1")
  r1 <- switch(view,
               R1[slice, , ],
               R1[, slice, ],
               R1[, , slice])
  r1[!mask] <- 0
  PD <- extract(qi, "PD")
  pd <- switch(view,
               PD[slice, , ],
               PD[, slice, ],
               PD[, , slice])
  pd[!mask] <- 0
  if (qi$model == 2) {
    MT <- extract(qi, "MT")
    delta <- switch(view,
                    MT[slice, , ],
                    MT[, slice, ],
                    MT[, , slice])
    delta[!mask] <- 0
  }
  indx <- 1:dim(r2star)[1]
  indy <- 1:dim(r2star)[2]

  if (qi$model == 2) {
    def.par <- par(mfrow = c(2, 2), mar = c(3, 3, 3, 0))
    rimage(indx, indy, r2star, zlim = c(0, 0.05), main = "R2star")
    rimage(indx, indy, r1, zlim = c(0.0002, 0.0015), main = "R1")
    rimage(indx, indy, pd, zlim = c(0, 10000), main = "PD")
    rimage(indx, indy, delta, zlim = c(0, 0.03), main = "MT")
  } else {
    def.par <- par(mfrow = c(2, 2), mar = c(3, 3, 3, 0))
    rimage(indx, indy, r2star, zlim = c(0, 0.05), main = "R2star")
    rimage(indx, indy, r1, zlim = c(0.0002, 0.0015), main = "R1")
    rimage(indx, indy, pd, zlim = c(0, 10000), main = "PD")
  }
  par(def.par)
}


writeQI <- function(qi,
                    dir = NULL,
                    prefix = "qmap",
                    verbose = TRUE) {

  if (!is.null(dir)) {
    if(!dir.exists(dir)) dir.create(dir)
    r2file <- file.path(dir, paste0(prefix,"R2"))
    r1file <- file.path(dir, paste0(prefix,"R1"))
    pdfile <- file.path(dir, paste0(prefix,"PD"))
    mtfile <- file.path(dir, paste0(prefix,"MT"))
  } else {
    r2file <- paste0(prefix,"R2")
    r1file <- paste0(prefix,"R1")
    pdfile <- paste0(prefix,"PD")
    mtfile <- paste0(prefix,"MT")
  }

  ds <- readNIfTI(qi$t1Files[1], reorient = FALSE)
  ds@datatype <- 16
  ds@magic <- "n+1"
  ds@vox_offset <- 352

  if (verbose) cat("writing R2 file ... ")
  ds@descrip <- "R2"
  writeNIfTI(as.nifti(extract(qi,"R2star"), ds), file = r2file)
  if (verbose) cat("done\n")
  if (verbose) cat("writing R1 file ... ")
  ds@descrip <- "R1"
  writeNIfTI(as.nifti(extract(qi,"R1"), ds), file = r1file)
  if (verbose) cat("done\n")
  if (verbose) cat("writing PD file ... ")
  ds@descrip <- "PD"
  writeNIfTI(as.nifti(extract(qi,"PD"), ds), file = pdfile)
  if (verbose) cat("done\n")
  if (qi$model == 2) {
    if (verbose) cat("writing MT file ... ")
    ds@descrip <- "MT"
    writeNIfTI(as.nifti(extract(qi,"MT"), ds), file = mtfile)
    if (verbose) cat("done\n")
  }
  invisible(NULL)
}

writeESTATICS <- function(mpmESTATICSModel,
                          dir = NULL,
                          prefix = "estatics",
                          verbose = TRUE) {

  if (!is.null(dir)) {
    if(!dir.exists(dir)) dir.create(dir)
    r2file <- file.path(dir, paste0(prefix,"R2"))
    st1file <- file.path(dir, paste0(prefix,"ST1"))
    spdfile <- file.path(dir, paste0(prefix,"SPD"))
    smtfile <- file.path(dir, paste0(prefix,"SMT"))
  } else {
    r2file <- paste0(prefix,"R2")
    st1file <- paste0(prefix,"ST1")
    spdfile <- paste0(prefix,"SPD")
    smtfile <- paste0(prefix,"SMT")
  }

  ds <- readNIfTI(mpmESTATICSModel$t1Files[1], reorient = FALSE)
  ds@datatype <- 16
  ds@magic <- "n+1"
  ds@vox_offset <- 352

  modelCoeff <- extract(mpmESTATICSModel,"modelCoeff")
  if (mpmESTATICSModel$model == 2) {
    if (verbose) cat("writing R2 file ... ")
    ds@descrip <- "R2"
    writeNIfTI(as.nifti(modelCoeff[4, , , ], ds), file = r2file)
    if (verbose) cat("done\n")
    if (verbose) cat("writing ST1 file ... ")
    ds@descrip <- "ST1"
    writeNIfTI(as.nifti(modelCoeff[1, , , ], ds), file = st1file)
    if (verbose) cat("done\n")
    if (verbose) cat("writing SPD file ... ")
    ds@descrip <- "SPD"
    writeNIfTI(as.nifti(modelCoeff[3, , , ], ds), file = spdfile)
    if (verbose) cat("done\n")
    if (verbose) cat("writing SMT file ... ")
    ds@descrip <- "SMT"
    writeNIfTI(as.nifti(modelCoeff[2, , , ], ds), file = smtfile)
    if (verbose) cat("done\n")
  } else {
    if (verbose) cat("writing R2 file ... ")
    ds@descrip <- "R2"
    writeNIfTI(as.nifti(modelCoeff[3, , , ], ds), file = r2file)
    if (verbose) cat("done\n")
    if (verbose) cat("writing ST1 file ... ")
    ds@descrip <- "ST1"
    writeNIfTI(as.nifti(modelCoeff[1, , , ], ds), file = st1file)
    if (verbose) cat("done\n")
    if (verbose) cat("writing SPD file ... ")
    ds@descrip <- "SPD"
    writeNIfTI(as.nifti(modelCoeff[2, , , ], ds), file = spdfile)
    if (verbose) cat("done\n")
  }

  if (!is.null(mpmESTATICSModel$smoothedData)) {
    smoothedData <- extract(mpmESTATICSModel,"smoothedData")
    ii <- 1
    t1Files <- mpmESTATICSModel$t1Files
    for (i in 1:length(t1Files)) {
      ds <- readNIfTI(t1Files[i], reorient = FALSE)
      ds@datatype <- 16
      ds@magic <- "n+1"
      ds@vox_offset <- 352
      if (!is.null(dir)) {
        fname <- file.path(dir, paste(prefix, basename(t1Files[i]), sep = ""))
      } else {
        fname <- paste(prefix, basename(t1Files[i]), sep = "")
      }
      if (verbose) cat("writing", fname, "... ")
      writeNIfTI(as.nifti(smoothedData[ii, , , ], ds), file = fname)
      if (verbose) cat("done\n")
      ii <- ii + 1
    }
    if (mpmESTATICSModel$model == 2) {
      mtFiles <- mpmESTATICSModel$mtFiles
      for (i in 1:length(mtFiles)) {
        ds <- readNIfTI(mtFiles[i], reorient = FALSE)
        ds@datatype <- 16
        ds@magic <- "n+1"
        ds@vox_offset <- 352
        if (!is.null(dir)) {
          fname <- file.path(dir, paste(prefix, basename(mtFiles[i]), sep = ""))
        } else {
          fname <- paste(prefix, basename(mtFiles[i]), sep = "")
        }
        if (verbose) cat("writing", fname, "... ")
        writeNIfTI(as.nifti(smoothedData[ii, , , ], ds), file = fname)
        if (verbose) cat("done\n")
        ii <- ii + 1
      }
    }
    for (i in 1:length(pdFiles)) {
      pdFiles <- mpmESTATICSModel$pdFiles
      ds <- readNIfTI(pdFiles[i], reorient = FALSE)
      ds@datatype <- 16
      ds@magic <- "n+1"
      ds@vox_offset <- 352
      if (!is.null(dir)) {
        fname <- file.path(dir, paste(prefix, basename(pdFiles[i]), sep = ""))
      } else {
        fname <- paste(prefix, basename(pdFiles[i]), sep = "")
      }
      if (verbose) cat("writing", fname, "... ")
      writeNIfTI(as.nifti(smoothedData[ii, , , ], ds), file = fname)
      if (verbose) cat("done\n")
      ii <- ii + 1
    }
  }
invisible(NULL)
}

estimateQIconf <- function(mpmESTATICSmodel,
                           verbose = TRUE) {

  if (mpmESTATICSmodel$model != 2) stop("only full model implemented!")

  ## TODO: this should be done in estimateESTATICS and smoothESTATICS
  dimnames(mpmESTATICSmodel$modelCoeff) <- list(c("ST1", "SMT", "SPD", "R2star"), NULL, NULL, NULL)
  dimnames(mpmESTATICSmodel$invCov) <- list(c("ST1", "SMT", "SPD", "R2star"), c("ST1", "SMT", "SPD", "R2star"), NULL, NULL, NULL)

  R1 <- array(0, mpmESTATICSmodel$sdim)
  CIR1 <- array(0, c(2, mpmESTATICSmodel$sdim))
  R2 <- array(0, mpmESTATICSmodel$sdim)
  CIR2 <- array(0, c(2, mpmESTATICSmodel$sdim))
  for (xyz in 1:sum(mpmESTATICSmodel$mask)){
  #for (z in 1:mpmESTATICSmodel$sdim[3]) {
  #  for (y in 1:mpmESTATICSmodel$sdim[2]) {
  #    for (x in 1:mpmESTATICSmodel$sdim[1]) {
  #      if (mpmESTATICSmodel$mask[x, y, z]) {
          if (is.null(mpmESTATICSmodel$bi)) {
            zz <- ESTATICS.confidence(mpmESTATICSmodel$modelCoeff[, xyz],
              mpmESTATICSmodel$invCov[, , xyz], mpmESTATICSmodel$FA[1]*pi/180,
              mpmESTATICSmodel$FA[length(mpmESTATICSmodel$t1Files) + length(mpmESTATICSmodel$mtFiles) + 1]*pi/180,
              mpmESTATICSmodel$TR[1], df=mpmESTATICSmodel$nFiles-4, 0.05)
          } else {
            zz <- ESTATICS.confidence(mpmESTATICSmodel$modelCoeff[, xyz],
              mpmESTATICSmodel$invCov[, , xyz] * mpmESTATICSmodel$bi[xyz],
              mpmESTATICSmodel$FA[1]*pi/180,
              mpmESTATICSmodel$FA[length(mpmESTATICSmodel$t1Files) + length(mpmESTATICSmodel$mtFiles) + 1]*pi/180,
              mpmESTATICSmodel$TR[1], df=mpmESTATICSmodel$nFiles-4, 0.05)
          }
          R2[xyz] <- zz$R2star
          R1[xyz] <- zz$R1
          CIR1[, xyz] <- zz$CIR1
          CIR2[, xyz] <- zz$CIR2star
#        }
#      }
#    }
    if (verbose) if(xyz%/%10000*10000==xyz) cat(xyz,"voxel completed  time", format(Sys.time()), "\n")
  }

  invisible(list(R1 = R1,
                 CIR1 = CIR1,
                 R2star = R2,
                 CIR2star = CIR2,
                 model = mpmESTATICSmodel$model,
                 t1Files = mpmESTATICSmodel$t1Files,
                 mtFiles = mpmESTATICSmodel$mtFiles,
                 pdFiles = mpmESTATICSmodel$pdFiles,
                 mask = mpmESTATICSmodel$mask))
}

writeQIconf <- function(qiConf,
                        dir= NULL,
                        verbose = TRUE) {
  if (!is.null(dir)) {
    if(!dir.exists(dir)) dir.create(dir)
    r1file <- file.path(dir, "R1")
    r1Lfile <- file.path(dir, "R1lower")
    r1Ufile <- file.path(dir, "R1upper")
    r2file <- file.path(dir, "R2")
    r2Lfile <- file.path(dir, "R2lower")
    r2Ufile <- file.path(dir, "R2upper")
  } else {
    r1file <- "R1"
    r1Lfile <- "R1lower"
    r1Ufile <- "R1upper"
    r2file <- "R2"
    r2Lfile <- "R2lower"
    r2Ufile <- "R2upper"
  }

  ds <- readNIfTI(qiConf$t1Files[1], reorient = FALSE)
  ds@datatype <- 16
  ds@magic <- "n+1"
  ds@vox_offset <- 352

  if (verbose) cat("writing R1 file ... ")
  ds@descrip <- "R1"
  R1 <- extract(qiConf, "R1")
  writeNIfTI(as.nifti(R1, ds), file = r1file)
  if (verbose) cat("done\n")
  if (verbose) cat("writing R1lower file ... ")
  ds@descrip <- "R1lower"
  CIR1 <- extract(qiConf, "CIR1")
  writeNIfTI(as.nifti(CIR1[1, , , ], ds), file = r1Lfile)
  if (verbose) cat("done\n")
  if (verbose) cat("writing R1upper file ... ")
  ds@descrip <- "R1upper"
  writeNIfTI(as.nifti(CIR1[2, , , ], ds), file = r1Ufile)
  if (verbose) cat("done\n")

  if (verbose) cat("writing R2 file ... ")
  ds@descrip <- "R2"
  R2star <- extract(qiConf,"R2star")
  writeNIfTI(as.nifti(R2star, ds), file = r2file)
  if (verbose) cat("done\n")
  if (verbose) cat("writing R2lower file ... ")
  ds@descrip <- "R2lower"
  CIR2star <- extract(qiConf,"CIR2star")
  writeNIfTI(as.nifti(CIR2star[1, , , ], ds), file = r2Lfile)
  if (verbose) cat("done\n")
  if (verbose) cat("writing R2upper file ... ")
  ds@descrip <- "R2upper"
  writeNIfTI(as.nifti(CIR2star[2, , , ], ds), file = r2Ufile)
  if (verbose) cat("done\n")

}

getPolynomsP2_ab <- function(TR_pdw, TR_t1w, fa_pdw, fa_t1w, verbose = TRUE) {

  ## Settings for R. Deichmann steady state correction using T2 = 64ms at 3T
  ## Correction parameters were calculated for 3 different parameter sets:
  if ((TR_pdw == 23.7) && (TR_t1w == 18.7) && (fa_pdw == 6) && (fa_t1w == 20)) {
    ## 1) classic FIL protocol (Weiskopf et al., Neuroimage 2011):
    ## PD-weighted: TR=23.7ms; a=6deg; T1-weighted: TR=18.7ms; a=20deg
    if (verbose) cat("Spoiling correction ... Classic FIL protocol\n")
    P2_a <- c(78.9228195006542,   -101.113338489192,    47.8783287525126)
    P2_b <- c(-0.147476233142129,    0.126487385091045,  0.956824374979504)
  } else if ((TR_pdw == 24.5) && (TR_t1w == 24.5) && (fa_pdw == 5) && (fa_t1w == 29)) {
    ## 2) new FIL/Helms protocol
    ## PD-weighted: TR=24.5ms; a=5deg; T1-weighted: TR=24.5ms; a=29deg
    if (verbose) cat("Spoiling correction ... New FIL/Helms protocol\n")
    P2_a <- c(93.455034845930480, -120.5752858196904,   55.911077913369060)
    P2_b <- c(-0.167301931434861,    0.113507432776106,  0.961765216743606)
  } else if ((TR_pdw == 24.0) && (TR_t1w == 19.0) && (fa_pdw == 6) && (fa_t1w == 20)) {
    ## 3) Siemens product sequence protocol used in Lausanne (G Krueger)
    ## PD-weighted: TR=24ms; a=6deg; T1-weighted: TR=19ms; a=20deg
    if (verbose) cat("Spoiling correction ... Siemens product Lausanne (GK) protocol\n")
    P2_a <- c(67.023102027100880, -86.834117103841540, 43.815818592349870)
    P2_b <- c(-0.130876849571103,   0.117721807209409,  0.959180058389875)
  } else if ((TR_pdw == 23.7) && (TR_t1w == 23.7) && (fa_pdw == 6) && (fa_t1w == 28)) {
    ## 4) High-res (0.8mm) FIL protocol:
    ## PD-weighted: TR=23.7ms; a=6deg; T1-weighted: TR=23.7ms; a=28deg
    if (verbose) cat("Spoiling correction ... High-res FIL protocol\n")
    P2_a <- c( 1.317257319014170e+02, -1.699833074433892e+02, 73.372595677371650)
    P2_b <- c(-0.218804328507184,      0.178745853134922,      0.939514554747592)
  } else if ((TR_pdw == 25.25) && (TR_t1w == 25.25) && (fa_pdw == 5) && (fa_t1w == 29)) {
    ## 4)NEW  High-res (0.8mm) FIL protocol:
    ## PD-weighted: TR=25.25ms; a=5deg; T1-weighted: TR=TR=25.25ms; a=29deg
    if (verbose) cat("Spoiling correction ... High-res FIL protocol\n")
    P2_a <- c(88.8623036106612,   -114.526218941363,    53.8168602253166)
    P2_b <- c(-0.132904017579521,    0.113959390779008,  0.960799295622202)
  } else if ((TR_pdw == 24.5) && (TR_t1w == 24.5) && (fa_pdw == 6) && (fa_t1w == 21)) {
    ## 5)NEW  1mm protocol - seq version v2k:
    ## PD-weighted: TR=24.5ms; a=6deg; T1-weighted: TR=24.5ms; a=21deg
    if (verbose) cat("Spoiling correction ... v2k protocol")
    P2_a <- c(71.2817617982844,   -92.2992876164017,   45.8278193851731)
    P2_b <- c(-0.137859046784839,   0.122423212397157,  0.957642744668469)
  } else if ((TR_pdw == 25.0) && (TR_t1w == 25.0) && (fa_pdw == 6) && (fa_t1w == 21)) {
    ## 6) 800um protocol - seq version v3* released used by MEG group:
    ## TR = 25ms for all volumes; flipAngles = [6, 21 deg] for PDw and T1w
    ## Correction parameters below were determined via Bloch-Torrey
    ## simulations but end result agrees well with EPG-derived correction
    ## for this RF spoiling increment of 137 degrees.
    ## See: Callaghan et al. ISMRM, 2015, #1694
    if (verbose) cat("Spoiling correction ... v3* 0.8mm R4 protocol\n")
    P2_a <- c(57.427573706259864, -79.300742898810441,  39.218584751863879)
    P2_b <- c(-0.121114060111119,   0.121684347499374,   0.955987357483519)
  } else {
    if (verbose) cat("Spoiling correction ... not defined for this protocol. No correction being applied.\n")
    P2_a <- c(0, 0, 0)
    P2_b <- c(0, 0, 1)
  }
  list(P2_a = P2_a, P2_b = P2_b)
}
