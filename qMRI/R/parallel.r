plmatrix <- function(x, FUN, ..., mc.cores = setCores(,reprt=FALSE)){
  dx <- dim(x)[2]
  if(mc.cores>dx) mc.cores <- dx
  n <- trunc((dx-1)/mc.cores)+1
  lx <- list(NULL)
  for(i in 1:(mc.cores-1)) lx[[i]] <- x[,(i-1)*n+1:n]
  lx[[mc.cores]] <- x[,((mc.cores-1)*n+1):dx]
  cl <- makeCluster(mc <- mc.cores)
  lz <- parLapply(cl, lx, FUN , ...)
  stopCluster(cl)
  z <- matrix(0,length(lz[[1]])/n, dx)
  for(i in 1:(mc.cores-1)) z[,(i-1)*n+1:n] <- lz[[i]]
  z[,((mc.cores-1)*n+1):dx] <- lz[[mc.cores]]
  z
}
# in: thetas , IRdataFluid, InvTimesScaled
# out: isConv, modelcoef
pIRfluid <- function(x, InvTimesScaled, method, sigma, CL, sig, L, varest, lower=c(0,0),
                     upper=c(2,2)){
   nvoxel <- dim(x)[2]
   npar <- 2
   ntimes <- length(InvTimesScaled)
   sind <- rep(1,ntimes)
   thetas <- x[1:2,]
   IRdataFluid <- x[-c(1:2),]
ergs <- array(0,c(npar+1,nvoxel))

for(xyz in 1:nvoxel){
  
  ivec <- IRdataFluid[, xyz]
  th <- thetas[, xyz]
  
  res <- if (method[1] == "NLR") try(nls(ivec ~ IRhomogen(par, InvTimesScaled),
                                         data = list(InvTimesScaled),
                                         start = list(par = th),
                                         control = list(maxiter = 200,
                                                        warnOnly = TRUE)),silent=TRUE)
  else try(nls(ivec ~ IRhomogenQL(par, InvTimesScaled, CL, sig, L),
               data = list(InvTimesScaled,
                           CL = CL,
                           sig = sig,
                           L = L),
               start = list(par = th),
               control = list(maxiter = 200,
                              warnOnly = TRUE)),silent=TRUE)
  if (!inherits(res, "try-error")){
    thhat <- coef(res)
    outofrange <- any(thhat != pmin(upper,pmax(lower,thhat)))
  }
  if (inherits(res, "try-error") || outofrange){
    # retry with port algorithm and bounds
    th <- pmin(upper,pmax(lower,th))
    res <- if (method[1] == "NLR") try(nls(ivec ~ IRhomogen(par, InvTimesScaled),
                                           data = list(InvTimes=InvTimesScaled),
                                           start = list(par = th),
                                           algorithm="port",
                                           control = list(maxiter = 200,
                                                          warnOnly = TRUE),
                                           lower=lower, upper=upper),silent=TRUE)
    else try(nls(ivec ~ IRhomogenQL(par, InvTimesScaled, CL, sig, L),
                 data = list(InvTimesScaled=InvTimesScaled,
                             CL = CL,
                             sig = sig,
                             L = L),
                 start = list(par = th),
                 algorithm="port",
                 control = list(maxiter = 200,
                                warnOnly = TRUE),
                 lower=lower, upper=upper),silent=TRUE)
  }
  if (!inherits(res, "try-error")) {
    sres <- if(varest[1]=="RSS") getnlspars(res) else
      getnlspars2(res, sigma, sind )
    ergs[npar+1,xyz] <- as.integer(res$convInfo$isConv)
    ergs[1:npar, xyz] <- sres$coefficients
  }
}
ergs
}

pIRsolid <- function(x, InvTimesScaled, Rfluid, Sfluid, method, sigma, CL, sig, L,
                     varest, 
                     lower=c(0,0,0),
                     upper=c(.95,2,2)){
  nvoxel <- dim(x)[2]
  npar <- 3
  thetas <- x[1:npar,]
  IRdataSolid <- x[-c(1:npar),]
  ntimes <- length(InvTimesScaled)
  sind <- rep(1,ntimes)
  df <- ntimes-3
  ergs <- array(0,c(npar+npar*npar+2,nvoxel))
  th1 <- (1:8)/10
  th2 <- Rfluid*c(.5,.6,.7,.8,.9,1.1,1.2)
  th3 <- Sfluid*(1:9)/10
  for(xyz in 1:nvoxel){
    
    ivec <- IRdataSolid[, xyz]
    th <- thetas[, xyz]
    ##
    ##   initialize using grid search and optim
    ##
    best <- LSIRmix2(th,ivec,InvTimesScaled,Sfluid,Rfluid)
    for(i in 1:8) for(j in 1:7) for(k in 1:9){
      z <- LSIRmix2(c(th1[i],th2[j],th3[k]),ivec,InvTimesScaled,Sfluid,Rfluid)
      if(z < best){
        best <- z 
        th <- c(th1[i],th2[j],th3[k])
      }
    }                
    th <- pmin(upper,pmax(lower,th))
    res <- if (method[1] == "NLR") try(optim(th, LSIRmix2, LSIRmix2grad, 
                                             Y=ivec, InvTimes=InvTimesScaled, S0f=Sfluid, Rf=Rfluid,
                                             method="L-BFGS-B",lower=lower,upper=upper))
    else try(optim(th, LSIRmix2QL, LSIRmix2QLgrad, 
                   Y=ivec, InvTimes=InvTimesScaled, S0f=Sfluid, Rf=Rfluid, 
                   CL = CL, sig = sig, L = L,
                   method="L-BFGS-B",lower=lower,upper=upper))
    
    if (!inherits(res, "try-error")){
      ergs[1:npar,xyz] <- th <- res$par
      ergs[npar+npar*npar+1,xyz] <- sqrt(res$value)
      ergs[npar+npar*npar+2,xyz] <- -res$convergence
    }
    res <- if (method[1] == "NLR") try(nls(ivec ~ IRmix2(par, ITS, Sfluid, Rfluid),
                                           data = list(ITS=InvTimesScaled, Sfluid=Sfluid, Rfluid=Rfluid),
                                           start = list(par = th),
                                           control = list(maxiter = 500,
                                                          warnOnly = TRUE)),silent=TRUE)
    else try(nls(ivec ~ IRmix2QL(par, ITS, Sfluid, Rfluid, CL, sig, L),
                 data = list(ITS=InvTimesScaled, Sfluid=Sfluid, Rfluid=Rfluid,
                             CL = CL, sig = sig, L = L),
                 start = list(par = th),
                 control = list(maxiter = 500,
                                warnOnly = TRUE)),silent=TRUE)
    if (!inherits(res, "try-error")){
      thhat <- coef(res)
      outofrange <- any(thhat != pmin(upper,pmax(lower,thhat)))
    }
    if (inherits(res, "try-error") || outofrange){
      # retry with port algorithm and bounds
      th <- pmin(upper,pmax(lower,th))
      res <- if (method[1] == "NLR") try(nls(ivec ~ IRmix2(par, ITS, Sfluid, Rfluid),
                                             data = list(ITS=InvTimesScaled, Sfluid=Sfluid, Rfluid=Rfluid),
                                             start = list(par = th),
                                             algorithm="port",
                                             control = list(maxiter = 500,
                                                            warnOnly = TRUE),
                                             lower=lower, upper=upper),silent=TRUE)
      else try(nls(ivec ~ IRmix2QL(par, ITS, Sfluid, Rfluid, CL, sig, L),
                   data = list(ITS=InvTimesScaled, Sfluid=Sfluid, Rfluid=Rfluid,
                               CL = CL, sig = sig, L = L),
                   start = list(par = th),
                   algorithm="port",
                   control = list(maxiter = 500,
                                  warnOnly = TRUE),
                   lower=lower, upper=upper),silent=TRUE)
    }
    if (!inherits(res, "try-error")) {
      sres <- if(varest[1]=="RSS") getnlspars(res) else
        getnlspars2(res, sigma, sind )
      ergs[npar+npar*npar+2,xyz] <- as.integer(res$convInfo$stopCode)
      ergs[1:npar, xyz] <- sres$coefficients
      if (sres$sigma != 0) {
        ergs[npar+1:(npar*npar) , xyz] <- as.vector(sres$invCov)*df
        ergs[npar+npar*npar+1,xyz] <- sres$sigma
      }
    }
  }  
  ergs
}

pEstESTATICS2 <- function(x,method,varest,xmat,wghts,maxR2star,L,lower,upper,sind){

#   ddata  :  x[1:ndata,]
#   thetas :  x[ndata+1:4,]
#   shat:     x[ndata+5:7,]
#   sigma:    x[ndata+8,]
#   CL:       x[ndata+9,]
npar <- 4
ndata <- dim(xmat)[1]
inddata   <- 1:ndata
indth     <- ndata+1:4
indshat   <- ndata+5:7
indsigma  <- ndata+8
indcl     <- ndata+9
nvoxel <- dim(x)[2]
ergs <- array(0,c(npar+npar*npar+2,nvoxel))
indcoeff <- 1:npar
indiCov <- npar+(1:(npar*npar))
indConv <- npar*(npar+1)+1
indrsig <- npar*(npar+1)+2
for(xyz in 1:nvoxel){
  
  if (method == "QL") {
      sig <- x[indsigma,xyz]
      CL <- x[indcl,xyz]
  }
  
  ivec <- x[inddata, xyz]#/dataScale
  th <- x[indth, xyz]
  
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
  
  if (!inherits(res, "try-error")) {
    sres <- if(varest=="RSS") getnlspars(res) else
      getnlspars2(res, x[indshat, xyz], sind )
    ergs[indConv,xyz] <- as.integer(res$convInfo$isConv)
    ergs[indcoeff, xyz] <- sres$coefficients
    if (sres$sigma != 0) {
      ergs[indiCov, xyz] <- as.vector(sres$invCov)
      ergs[indrsig,xyz] <- sres$sigma
    }
  }
  
  if (inherits(res, "try-error") || coef(res)[npar] > maxR2star || coef(res)[npar] < 0) {
    
    ## fallback for not converged or R2star out of range
    sres <- if(varest=="RSS") linearizedESTATICS(ivec, xmat, maxR2star, wghts) else
      linearizedESTATICS2(ivec, xmat, maxR2star, x[indshat, xyz], sind ,wghts)
    ## thats already the solution for NLR if R2star is fixed
    isThresh <- sres$invCov[npar, npar] == 0
    ergs[indConv,xyz] <- 255 ## partially linearized NLR model
    xmat0 <- sres$xmat
    th <- sres$theta
    ergs[1:(npar-1), xyz] <- sres$theta
    ergs[npar, xyz] <- sres$R2star
    if (sres$sigma2 != 0) {
      ergs[indiCov , xyz] <- as.vector(sres$invCov)
      ergs[indrsig,xyz] <- sqrt(sres$sigma2)
    }
    
    if (method == "QL") {
      xmat0 <- sres$xmat
      # xmat0 containes design matrix for linear problem with fixed R2star
      # ony have nonlinearity from QL
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
       if (!inherits(res,"try-error")) {
        ergs[indConv,xyz] <- as.integer(res$convInfo$isConv)
        sres <- getnlspars(res)
        ergs[1:(npar-1), xyz] <- sres$coefficients
        if (sres$sigma != 0) {
          invCovtmp <- sres$XtX
          ergs[indiCov[c(1:3,5:7,9:11)], xyz] <- as.vector(invCovtmp/sres$sigma^2)
          ergs[indrsig,xyz] <- sres$sigma
        }
      } else {
        ergs[indConv,xyz] <- -1
      }
    }
  }}
  ergs
}

pEstESTATICS1 <- function(x,method,varest,xmat,wghts,maxR2star,L,lower,upper,sind){
  
  
#   ddata  :  x[1:ndata,]
#   thetas :  x[ndata+1:3,]
#   shat:     x[ndata+4:5,]
#   sigma:    x[ndata+6,]
#   CL:       x[ndata+7,]
npar <- 3
ndata <- dim(xmat)[1]
inddata   <- 1:ndata
indth     <- ndata+1:3
indshat   <- ndata+4:5
indsigma  <- ndata+6
indcl     <- ndata+7
nvoxel <- dim(x)[2]
ergs <- array(0,c(npar+npar*npar+2,nvoxel))
indcoeff <- 1:npar
indiCov <- npar+(1:(npar*npar))
indConv <- npar*(npar+1)+1
indrsig <- npar*(npar+1)+2
  for(xyz in 1:nvoxel){
    
    if (method == "QL") {
        sig <- x[indsigma,xyz]
        CL <- x[indcl,xyz]
    }
    
    ivec <- x[inddata, xyz]#/dataScale
    th <- x[indth, xyz]
    
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

    if (!inherits(res, "try-error")) {
      sres <- if(varest=="RSS") getnlspars(res) else
        getnlspars2(res, x[indshat, xyz], sind )
      ergs[indConv,xyz] <- as.integer(res$convInfo$isConv)
      ergs[indcoeff, xyz] <- sres$coefficients
      if (sres$sigma != 0) {
        ergs[indiCov,  xyz] <- as.vector(sres$invCov)
        ergs[indrsig, xyz] <- sres$sigma
      }
    }
    
    if (inherits(res, "try-error") || coef(res)[npar] > maxR2star || coef(res)[npar] < 0) {
      
      ## fallback for not converged or R2star out of range
      sres <- if(varest=="RSS") linearizedESTATICS(ivec, xmat, maxR2star, wghts) else
        linearizedESTATICS2(ivec, xmat, maxR2star, x[indshat, xyz], sind ,wghts)
      ## thats already the solution for NLR if R2star is fixed
      isThresh <- sres$invCov[npar, npar] == 0
      ergs[indConv, xyz] <- 255 ## partially linearized NLR model
      xmat0 <- sres$xmat
      th <- sres$theta
      ergs[1:(npar-1), xyz] <- sres$theta
      ergs[npar, xyz] <- sres$R2star
      if (sres$sigma2 != 0) {
        ergs[indiCov , xyz] <- as.vector(sres$invCov)
        ergs[indrsig, xyz] <- sqrt(sres$sigma2)
      }
      
      if (method == "QL") {
        xmat0 <- sres$xmat
        # xmat0 containes design matrix for linear problem with fixed R2star
        # ony have nonlinearity from QL
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
        if (!inherits(res,"try-error")) {
          ergs[indConv, xyz] <- as.integer(res$convInfo$isConv)
          sres <- getnlspars(res)
          ergs[1:(npar-1), xyz] <- sres$coefficients
          if (sres$sigma != 0) {
            invCovtmp <- sres$XtX
            ergs[indiCov[c(1:2,4:5)], xyz] <- invCovtmp/sres$sigma^2
            ergs[indrsig, xyz] <- sres$sigma
          }
        } else {
          ergs[indConv, xyz] <- -1
        }
      }
    }}
    ergs
}

pEstESTATICS0 <- function(x,method,varest,xmat,wghts,maxR2star,L,lower,upper,sind){
  
#   ddata  :  x[1:ndata,]
#   thetas :  x[ndata+1:2,]
#   shat:     x[ndata+3,]
#   sigma:    x[ndata+4,]
#   CL:       x[ndata+5,]
npar <- 2
ndata <- dim(xmat)[1]
inddata   <- 1:ndata
indth     <- ndata+1:2
indshat   <- ndata+3
indsigma  <- ndata+4
indcl     <- ndata+5
nvoxel <- dim(x)[2]
ergs <- array(0,c(npar+npar*npar+2,nvoxel))
indcoeff <- 1:npar
indiCov <- npar+(1:(npar*npar))
indConv <- npar*(npar+1)+1
indrsig <- npar*(npar+1)+2
  for(xyz in 1:nvoxel){
    
    if (method == "QL") {
        sig <- x[indsigma,xyz]
        CL <- x[indcl,xyz]
    }
    
    ivec <- x[inddata, xyz]#/dataScale
    th <- x[indth, xyz]
    

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
    
    if (!inherits(res, "try-error")) {
      sres <- if(varest=="RSS") getnlspars(res) else
        getnlspars2(res, x[indshat, xyz], sind )
      ergs[indConv, xyz] <- as.integer(res$convInfo$isConv)
      ergs[indcoeff, xyz] <- sres$coefficients
      if (sres$sigma != 0) {
        ergs[indiCov , xyz] <- as.vector(sres$invCov)
        ergs[indrsig, xyz] <- sres$sigma
      }
    }
    
    if (inherits(res, "try-error") || coef(res)[npar] > maxR2star || coef(res)[npar] < 0) {
      
      ## fallback for not converged or R2star out of range
      sres <- if(varest=="RSS") linearizedESTATICS(ivec, xmat, maxR2star, wghts) else
        linearizedESTATICS2(ivec, xmat, maxR2star, x[indshat, xyz], sind ,wghts)
      ## thats already the solution for NLR if R2star is fixed
      isThresh <- sres$invCov[npar, npar] == 0
      ergs[indConv, xyz] <- 255 ## partially linearized NLR model
      xmat0 <- sres$xmat
      th <- sres$theta
      ergs[1:(npar-1), xyz] <- sres$theta
      ergs[npar, xyz] <- sres$R2star
      if (sres$sigma2 != 0) {
        ergs[indiCov , xyz] <- as.vector(sres$invCov)
        ergs[indrsig, xyz] <- sqrt(sres$sigma2)
      }
      
      if (method == "QL") {
        xmat0 <- sres$xmat
        # xmat0 containes design matrix for linear problem with fixed R2star
        # ony have nonlinearity from QL

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
          ergs[indConv, xyz] <- as.integer(res$convInfo$isConv)
          sres <- getnlspars(res)
          ergs[1:(npar-1), xyz] <- sres$coefficients
          if (sres$sigma != 0) {
            invCovtmp <- sres$XtX
            ergs[indiCov[1], xyz] <- invCovtmp/sres$sigma^2
            ergs[indrsig, xyz] <- sres$sigma
          }
        } else {
          ergs[indConv, xyz] <- -1
        }
      }
    }}
    ergs
}
