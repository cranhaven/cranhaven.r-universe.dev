readIRData <- function(t1Files,InvTimes,segmFile,sigma=NULL,L=1,
       segmCodes=c("GM","WM","CSF")){
   if (is.null(t1Files)) stop("vector of T1 files required")
   nFiles <- length(t1Files)
   if(length(InvTimes) != nFiles)
    stop("readIRData: t1Files and InvTimes have different lengths")
   sdim <- dim(readNIfTI(t1Files[1], read_data = FALSE))
   s1 <- (1:3)[segmCodes=="CSF"]
   s2 <- (1:3)[segmCodes=="GM"]
   s3 <- (1:3)[segmCodes=="WM"]
   segm <- c1 <- readNIfTI(segmFile[s1],reorient=FALSE)@.Data # CSF
   if(length(segmFile) == 1){
# reorder tissue codes such that "CSF", "GM" and "WM" are coded as 1:3
      if(any(segmCodes!=c("CSF","GM","WM"))){
         segm0 <- segm
         segm[segm0==s1] <- 1
         segm[segm0==s2] <- 2
         segm[segm0==s3] <- 3
      }
   } else if(length(segmFile) == 3){
# segmFiles contain probability maps for CSF, GM, WM as specified in segmCodes
      c2 <- readNIfTI(segmFile[s2],reorient=FALSE) # GM
      c3 <- readNIfTI(segmFile[s3],reorient=FALSE) # WM
      segm <- array(0,dim(c1))
      segm[c1 >= pmax(1/3,c1,c2,c3)] <- 1
      segm[c2 >= pmax(1/3,c1,c2,c3)] <- 2
      segm[c3 >= pmax(1/3,c1,c2,c3)] <- 3
   }
   if(any(dim(segm)!=sdim)) stop("readIRData: dimensions of t1Files and segmFiles are incompatible")
# in segm 1 codes CSF, 2 codes GM, 3 codes WM
   IRdata <- array(0,c(nFiles,sdim))
   for(i in 1:nFiles) IRdata[i,,,] <- readNIfTI(t1Files[i],reorient=FALSE)
   InvTimes[is.infinite(InvTimes)] <- 10 * max(InvTimes[is.finite(InvTimes)])
   if(is.null(sigma)){
      ind <- (InvTimes == max(InvTimes))
      ddata <- IRdata[ind,,,]
      if(length(dim(ddata))==4) ddata<-ddata[1,,,] # multiple max(InvTimes)
      shat <- awsLocalSigma(ddata, steps=16,
                           mask=(segm==1), ncoils=L, hsig=2.5,
                           lambda=6,family="Gauss")$sigma
      dim(shat) <- sdim
      shat <- shat[segm==1]
      sigma <- median(shat)
   }
   data <- list(IRdata=IRdata, InvTimes=InvTimes, segm=segm, sigma=sigma, L=L)
   class(data) <- "IRdata"
   data
}


estimateIRfluid <- function(IRdataobj,
                            TEScale = 100,
                            dataScale = 1000,
                            method = c("NLR", "QL"),
                            varest = c("RSS","data"),
                            verbose = TRUE,
                            lower=c(0,0),
                            upper=c(2,2)){
   IRdata <- IRdataobj$IRdata
   InvTimes <- IRdataobj$InvTimes
   segm <- IRdataobj$segm
   sigma <- IRdataobj$sigma
   L <- IRdataobj$L
   mask <- segm==1
   nvoxel <- sum(mask)
   ntimes <- length(InvTimes)
   sind <- rep(1,ntimes)
   itmax <- order(InvTimes)[ntimes]
   dimdata <- dim(IRdata)
   InvTimesScaled <- InvTimes/TEScale
   ## create necessary arrays
   npar <- 2 #  th2 for R, th1 for S
   Rx <- Sx <- Convx <- array(0,dim(mask))
   isConv <- array(0, nvoxel)
   isThresh <- array(FALSE, nvoxel)
   modelCoeff <- array(0, c(npar, nvoxel))
   if(varest[1]=="data"){
     if(verbose) cat("estimating variance maps from data\n")
     ind <- (InvTimes == max(InvTimes))[1]
     ddata <- IRdata[ind,,,]
     shat <- awsLocalSigma(ddata, steps=16,
                                        mask=(segm==1), ncoils=1, hsig=2.5,
                                        lambda=6,family="Gauss")$sigma
     dim(shat) <- dimdata[-1]
     shat <- shat[segm==1]
     shat[shat==0] <- quantile(shat,.8)
   }
   if (method[1] == "QL") {
     sig <- sigma/dataScale
     CL <- sig * sqrt(pi/2) * gamma(L + 0.5)/gamma(L)/gamma(1.5)
   } else {
     # just declare for parallel call
     sig <- 1
     CL <- 1
   }
   # initial parameters
     dim(IRdata) <- c(dimdata[1],prod(dim(segm)))
     IRdataFluid <- IRdata[,segm==1]
     thetas <- matrix(0,2,nvoxel)
     order1 <- function(x) order(x)[1]
     itmin <- apply(IRdataFluid,2,order1)
     thetas[1,] <- IRdataFluid[itmax,]/dataScale
     thetas[2,] <- log(2)/InvTimes[itmin]*TEScale
     if (verbose){
        cat("Start estimation in", nvoxel, "voxel at", format(Sys.time()), "\n")
        if(setCores()==1) pb <- txtProgressBar(0, nvoxel, style = 3)
     }
     if( setCores() >1){
       x <- array(0,c(ntimes+2,nvoxel))
       x[1:2,] <- thetas
       x[-(1:2),] <- IRdataFluid/dataScale
 #      ergs <- array(0, c(npar+1,nvoxel)) 
       ergs <- plmatrix(x,pIRfluid,InvTimesScaled, method, 
                        sigma, CL, sig, L, varest, lower, upper)
       modelCoeff <- ergs[1:npar,]
       isConv <- ergs[npar+1,]
     }  else
     for(xyz in 1:nvoxel){
     
     ivec <- IRdataFluid[, xyz]/dataScale
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
         isConv[xyz] <- as.integer(res$convInfo$isConv)
         modelCoeff[, xyz] <- sres$coefficients
       }
       if (verbose) if(xyz%/%1000*1000==xyz) setTxtProgressBar(pb, xyz)
     }
  Rx[mask] <- modelCoeff[2,]
  Sx[mask] <- modelCoeff[1,]
  Convx[mask] <- isConv
  Sf <- median(modelCoeff[1,],na.rm=TRUE)
  Rf <- median(modelCoeff[2,],na.rm=TRUE)
  if (verbose){
    if(setCores()==1) close(pb)
    cat("Finished estimation", format(Sys.time()), "\n","Sf",Sf,"Rf",Rf,"\n")
  }
  dim(IRdata) <- dimdata
  # Results are currently scaled by TEscale (R) and Datascale (S)
  z <- list(IRdata=IRdata, InvTimes=InvTimes, segm=segm, sigma=sigma, L=L,Sf=Sf*dataScale,Rf=Rf/TEScale,Sx=Sx,Rx=Rx,
  Convx=Convx,method=method,varest=varest)
  class(z) <- "IRfluid"
  z
}
   


estimateIRsolid <- function(IRfluidobj,
                            TEScale = 100,
                            dataScale = 1000,
                            verbose = TRUE,
                            lower=c(0,0,0),
                            upper=c(.95,2,2)){
   IRdata <- IRfluidobj$IRdata
   InvTimes <- IRfluidobj$InvTimes
   segm <- IRfluidobj$segm
   sigma <- IRfluidobj$sigma
   L <- IRfluidobj$L
   Sfluid <- IRfluidobj$Sf/dataScale
   Rfluid <- IRfluidobj$Rf*TEScale
   method <- IRfluidobj$method
   varest <- IRfluidobj$varest
   mask <- segm>1
   nvoxel <- sum(mask)
   ntimes <- length(InvTimes)
   df <- ntimes-3
   InvTimes[InvTimes==Inf] <- 50*max(InvTimes[InvTimes!=Inf])
   sind <- rep(1,ntimes)
   dimdata <- dim(IRdata)
   if(dimdata[1]!=ntimes) stop("estimateIRsolid: incompatible length of InvTimes")
   if(any(dimdata[-1]!=dim(mask))) stop("estimateIRsolid: incompatible dimension of segm")
   InvTimesScaled <- InvTimes/TEScale
   ## create necessary arrays
   npar <- 3 # th1 for f, th2 for R, th3 for S
   fx <- Rx <- Sx <- rsdx <- array(0,dim(mask))
   ICovx <- array(0,c(3,3,prod(dim(mask))))
   Convx <- array(0,dim(mask))
   fx[segm==1] <- 1
   Rx[segm==1] <- Rfluid
   Sx[segm==1] <- Sfluid
   Convx[segm==1] <- 1
   # set ICovx for fluid as (numerically) diag(rep(Inf),3)
   ICovx[1,1,segm==1] <- 1e20
   ICovx[2,2,segm==1] <- 1e20
   ICovx[3,3,segm==1] <- 1e20
   isConv <- array(FALSE, nvoxel)
   isThresh <- array(FALSE, nvoxel)
   modelCoeff <- array(0, c(npar, nvoxel))
   invCov <- array(0, c(npar, npar, nvoxel))
   rsigma <- array(0, nvoxel)
   if (method[1] == "QL") {
         sig <- sigma/dataScale
         CL <- sig * sqrt(pi/2) * gamma(L + 0.5)/gamma(L)/gamma(1.5)
   } else {
     # just declare for parallel call, won't be used
     sig <- 1
     CL <- 1
   }
      # initial parameters
      dim(IRdata) <- c(dimdata[1],prod(dim(segm)))
      IRdataSolid <- IRdata[,mask]
      thetas <- matrix(0,3,nvoxel)
      thetas[3,] <- IRdataSolid[(1:ntimes)[InvTimes == max(InvTimes)][1],]/dataScale
      thetas[2,] <- min(upper[2],max(lower[2],10/median(InvTimesScaled)))
      thetas[1,] <- 0.3
      if (verbose){
         cat("Start estimation in", nvoxel, "voxel at", format(Sys.time()), "\n")
         if(setCores() == 1) pb <- txtProgressBar(0, nvoxel, style = 3)
      }
      if( setCores() >1){
        x <- array(0,c(ntimes+npar,nvoxel))
        x[1:npar,] <- thetas
        x[-(1:npar),] <- IRdataSolid/dataScale
 #       ergs <- array(0, c(npar+npar*npar+2,nvoxel)) 
        ergs <- plmatrix(x,pIRsolid,InvTimesScaled, Rfluid, Sfluid, method,
                         sigma, CL, sig, L, varest, lower, upper)
        isConv <- ergs[npar+npar*npar+2,]
        modelCoeff <- ergs[1:npar, ] 
        invCov <- ergs[npar+1:(npar*npar), ] 
        dim(invCov) <- c(npar,npar,nvoxel)
        rsigma <- ergs[npar+npar*npar+1,]
      }  else {
      th1 <- (1:8)/10
      th2 <- Rfluid*c(.5,.6,.7,.8,.9,1.1,1.2)
      th3 <- Sfluid*(1:9)/10
      for(xyz in 1:nvoxel){
         
         ivec <- IRdataSolid[, xyz]/dataScale
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
           modelCoeff[,xyz] <- th <- res$par
           rsigma[xyz] <- sqrt(res$value)
           isConv[xyz] <- -res$convergence
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
            isConv[xyz] <- as.integer(res$convInfo$stopCode)
            modelCoeff[, xyz] <- sres$coefficients
            if (sres$sigma != 0) {
               invCov[, , xyz] <- sres$invCov*df
               rsigma[xyz] <- sres$sigma
            }
         }
         if (verbose) if(xyz%/%1000*1000==xyz) setTxtProgressBar(pb, xyz)
      }
      }
      if (verbose){
        if(setCores(1)==1) close(pb)
        cat("Finished estimation", format(Sys.time()), "\n")
      }
      fx[mask] <- modelCoeff[1,]
      Rx[mask] <- modelCoeff[2,]
      Sx[mask] <- modelCoeff[3,]
      ICovx[,,mask] <- invCov
      Convx[mask] <- isConv
      rsdx[mask] <- rsigma
      ICovx[1,2,] <- ICovx[2,1,] <- ICovx[1,2,]*TEScale
      ICovx[1,3,] <- ICovx[3,1,] <- ICovx[1,3,]/dataScale
      ICovx[2,2,] <- ICovx[2,2,]*TEScale*TEScale
      ICovx[2,3,] <- ICovx[3,2,] <- ICovx[2,3,]/dataScale*TEScale
      ICovx[3,3,] <- ICovx[3,3,]/dataScale/dataScale
      dim(ICovx) <- c(3,3,dim(mask))
      dim(IRdata) <- dimdata
      
# Results are currently scaled by TEScale (R) and dataScale (S)
      z <- list(IRdata=IRdata, InvTimes=InvTimes, segm=segm, sigma=sigma, L=L,
                fx=fx,Rx=Rx/TEScale,Sx=Sx*dataScale,Sf=Sfluid*dataScale,
                Rf=Rfluid/TEScale,ICovx=ICovx,Convx=Convx,rsdx=rsdx,
                method=method,varest=varest)
      class(z) <- "IRmixed"
      z
}


estimateIRsolid2 <- function(IRfluidobj, InvTimes, segm, Sfluid, Rfluid,
                            TEScale = 100,
                            dataScale = 1000,
                            method = c("NLR", "QL"),
                            sigma = NULL,
                            L = 1,
                            varest = c("RSS","data"),
                            verbose = TRUE,
                            lower=c(0,0,0),
                            upper=c(.95,2,2)){
   IRdata <- IRfluidobj$IRdata
   InvTimes <- IRfluidobj$InvTimes
   segm <- IRfluidobj$segm
   sigma <- IRfluidobj$sigma
   L <- IRfluidobj$L
   Sfluid <- IRfluidobj$Sf/dataScale
   Rfluid <- IRfluidobj$Rf*TEScale
   method <- IRfluidobj$method
   varest <- IRfluidobj$varest
   mask <- segm>1
   nvoxel <- sum(mask)
   ntimes <- length(InvTimes)
   InvTimes[InvTimes==Inf] <- 50*max(InvTimes[InvTimes!=Inf])
   sind <- rep(1,ntimes)
   dimdata <- dim(IRdata)
   if(dimdata[1]!=ntimes) stop("estimateIRsolid: incompatible length of InvTimes")
   if(any(dimdata[-1]!=dim(mask))) stop("estimateIRsolid: incompatible dimension of segm")
   InvTimesScaled <- InvTimes/TEScale
   ## create necessary arrays
   npar <- 3 # th1 for f, th2 for R, th3 for S
   df <- length(InvTimes)-npar
   fx <- Rx <- Sx <- rsdx <- array(0,dim(mask))
   ICovx <- array(0,c(3,3,prod(dim(mask))))
   Convx <- array(0,dim(mask))
   fx[segm==1] <- 1
   Rx[segm==1] <- Rfluid
   Sx[segm==1] <- Sfluid
   Convx[segm==1] <- 1
   # set ICovx for fluid as (numerically) diag(rep(Inf),3)
   ICovx[1,1,segm==1] <- 1e20
   ICovx[2,2,segm==1] <- 1e20
   ICovx[3,3,segm==1] <- 1e20
   isConv <- array(FALSE, nvoxel)
   isThresh <- array(FALSE, nvoxel)
   modelCoeff <- array(0, c(npar, nvoxel))
   invCov <- array(0, c(npar, npar, nvoxel))
   rsigma <- array(0, nvoxel)
   if (method[1] == "QL") {
      sig <- sigma/dataScale
      CL <- sig * sqrt(pi/2) * gamma(L + 0.5)/gamma(L)/gamma(1.5)
   }
   # initial parameters
   dim(IRdata) <- c(dimdata[1],prod(dim(segm)))
   IRdataSolid <- IRdata[,mask]
   thetas <- matrix(0,3,nvoxel)
   thetas[3,] <- IRdataSolid[(1:ntimes)[InvTimes == max(InvTimes)][1],]/dataScale
   thetas[2,] <- min(upper[2],max(lower[2],10/median(InvTimesScaled)))
   thetas[1,] <- 0.3
   if (verbose){
      cat("Start estimation in", nvoxel, "voxel at", format(Sys.time()), "\n")
      pb <- txtProgressBar(0, nvoxel, style = 3)
   }
   th1 <- (1:8)/10
   th2 <- Rfluid*c(.5,.6,.7,.8,.9,1.1,1.2)
   th3 <- Sfluid*(1:9)/10
   for(xyz in 1:nvoxel){
      
      ivec <- IRdataSolid[, xyz]/dataScale
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
         modelCoeff[,xyz] <- th <- res$par
         x <- attr(if(method[1]=="NLR") IRmix2(th,InvTimesScaled,Sfluid,Rfluid) 
                   else IRmix2QL(th,InvTimesScaled,Sfluid,Rfluid,CL,sig,L),"grad") 
         rsigma[xyz] <- sqrt(res$value/df)
         isConv[xyz] <- res$convergence
         invCov[,,xyz] <- t(x)%*%x/res$value*df
      }
      if (verbose) if(xyz%/%1000*1000==xyz) setTxtProgressBar(pb, xyz)
   }
   if (verbose){
      close(pb)
      cat("Finished estimation", format(Sys.time()), "\n")
   }
   fx[mask] <- modelCoeff[1,]
   Rx[mask] <- modelCoeff[2,]
   Sx[mask] <- modelCoeff[3,]
   ICovx[,,mask] <- invCov
   Convx[mask] <- isConv
   rsdx[mask] <- rsigma
      ICovx[1,2,] <- ICovx[2,1,] <- ICovx[1,2,]*TEScale
      ICovx[1,3,] <- ICovx[3,1,] <- ICovx[1,3,]/dataScale
      ICovx[2,2,] <- ICovx[2,2,]*TEScale*TEScale
      ICovx[2,3,] <- ICovx[3,2,] <- ICovx[2,3,]/dataScale*TEScale
      ICovx[3,3,] <- ICovx[3,3,]/dataScale/dataScale
   dim(ICovx) <- c(3,3,dim(mask))
   dim(IRdata) <- dimdata
# Results are currently scaled by TEScale (R) and dataScale (S)
      z <- list(IRdata=IRdata, InvTimes=InvTimes, segm=segm, sigma=sigma, L=L,
                fx=fx,Rx=Rx/TEScale,Sx=Sx*dataScale,Sf=Sfluid*dataScale,
                Rf=Rfluid/TEScale,ICovx=ICovx,Convx=Convx,rsdx=rsdx,
                method=method,varest=varest)
      class(z) <- "IRmixed"
      z
}

estimateIRfull <- function(IRsolidobj,
                            TEScale = 100,
                            dataScale = 1000,
                            verbose = TRUE,
                            lower=c(0,0,0,0,0),
                            upper=c(.95,2,2,2,2)){
IRdata <- IRsolidobj$IRdata
  InvTimes <- IRsolidobj$InvTimes
  segm <- IRsolidobj$segm
  sigma <- IRsolidobj$sigma
  L <- IRsolidobj$L
  Sx <- IRsolidobj$Sx/dataScale
  Rx <- IRsolidobj$Rx*TEScale
  fx <- IRsolidobj$fx
  Sfluid <- IRsolidobj$Sf/dataScale
  Rfluid <- IRsolidobj$Rf*TEScale
  Sf <- array(Sfluid,dim(Sx))
  Rf <- array(Rfluid,dim(Sx))
  Sf[segm==1] <- Sx[segm==1]
  Rf[segm==1] <- Rx[segm==1]
  method <- IRsolidobj$method
  varest <- IRsolidobj$varest
  mask <- segm>1
  nvoxel <- sum(mask)
  ntimes <- length(InvTimes)
  InvTimes[InvTimes==Inf] <- 50*max(InvTimes[InvTimes!=Inf])
  sind <- rep(1,ntimes)
  dimdata <- dim(IRdata)
  if(dimdata[1]!=ntimes) stop("estimateIRsolid: incompatible length of InvTimes")
  if(any(dimdata[-1]!=dim(mask))) stop("estimateIRsolid: incompatible dimension of segm")
  InvTimesScaled <- InvTimes/TEScale
   ## create necessary arrays
  npar <- 5 # th1 for f, th2 for R_s, th3 for S_s, , th4 for R_f, th5 for S_f
  df <- length(InvTimes)-npar
  rsdx <- array(0,dim(mask))
  ICovx <- array(0,c(5,5,prod(dim(mask))))
  Convx <- array(0,dim(mask))
  Convx[segm==1] <- 1
  # set ICovx for fluid as (numerically) diag(rep(Inf),3)
  ICovx[1,1,segm==1] <- 1e20
  ICovx[2,2,segm==1] <- 1e20
  ICovx[3,3,segm==1] <- 1e20
  ICovx[4,4,segm==1] <- 1e20
  ICovx[5,5,segm==1] <- 1e20
  isConv <- array(FALSE, nvoxel)
  isThresh <- array(FALSE, nvoxel)
  modelCoeff <- array(0, c(npar, nvoxel))
  invCov <- array(0, c(npar, npar, nvoxel))
  rsigma <- array(0, nvoxel)
   if (method[1] == "QL") {
         sig <- sigma/dataScale
         CL <- sig * sqrt(pi/2) * gamma(L + 0.5)/gamma(L)/gamma(1.5)
      }
      # initial parameters
  dim(IRdata) <- c(dimdata[1],prod(dim(segm)))
  IRdataSolid <- IRdata[,mask]
  thetas <- matrix(0,npar,nvoxel)
  thetas[5,] <- Sf[mask]
  thetas[4,] <- Rf[mask]
  thetas[3,] <- Sx[mask]
  thetas[2,] <- Rx[mask]
  thetas[1,] <- fx[mask]
      if (verbose){
         cat("Start estimation in", nvoxel, "voxel at", format(Sys.time()), "\n")
         pb <- txtProgressBar(0, nvoxel, style = 3)
      }
      for(xyz in 1:nvoxel){

         ivec <- IRdataSolid[, xyz]/dataScale
         th <- thetas[, xyz]
##
##   initialize using grid search and optim
##
         th <- pmin(upper,pmax(lower,th))
                           res <- if (method[1] == "NLR") try(optim(th, LSIRmix5, LSIRmix5grad,
                                                    Y=ivec, InvTimes=InvTimesScaled,
                                                    method="L-BFGS-B",lower=lower,upper=upper))
                           else try(optim(th, LSIRmix5QL, LSIRmix5QLgrad,
                                          Y=ivec, InvTimes=InvTimesScaled,
                                          CL = CL, sig = sig, L = L,
                                          method="L-BFGS-B",lower=lower,upper=upper))

                  if (!inherits(res, "try-error")){
           modelCoeff[,xyz] <- th <- res$par
           rsigma[xyz] <- sqrt(res$value)
           isConv[xyz] <- -res$convergence
         }
         res <- if (method[1] == "NLR") try(nls(ivec ~ IRmix5(par, ITS),
                                             data = list(ITS=InvTimesScaled),
                                             start = list(par = th),
                                             control = list(maxiter = 500,
                                                            warnOnly = TRUE)),silent=TRUE)
         else try(nls(ivec ~ IRmix5QL(par, ITS, CL, sig, L),
                      data = list(ITS=InvTimesScaled,
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
            res <- if (method[1] == "NLR") try(nls(ivec ~ IRmix5(par, ITS),
                                                data = list(ITS=InvTimesScaled),
                                                start = list(par = th),
                                                algorithm="port",
                                                control = list(maxiter = 500,
                                                               warnOnly = TRUE),
                                                lower=lower, upper=upper),silent=TRUE)
            else try(nls(ivec ~ IRmix5QL(par, ITS, CL, sig, L),
                                     data = list(ITS=InvTimesScaled,
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
            isConv[xyz] <- as.integer(res$convInfo$stopCode)
            modelCoeff[, xyz] <- sres$coefficients
            if (sres$sigma != 0) {
               invCov[, , xyz] <- sres$invCov*df
               rsigma[xyz] <- sres$sigma
            }
         }
         if (verbose) if(xyz%/%1000*1000==xyz) setTxtProgressBar(pb, xyz)
      }
      if (verbose){
        close(pb)
        cat("Finished estimation", format(Sys.time()), "\n")
      }
      fx[mask] <- modelCoeff[1,]
      Rx[mask] <- modelCoeff[2,]
      Sx[mask] <- modelCoeff[3,]
      Rf[mask] <- modelCoeff[4,]
      Sf[mask] <- modelCoeff[5,]
      ICovx[,,mask] <- invCov
      Convx[mask] <- isConv
      rsdx[mask] <- rsigma
  ICovx[1,2,] <- ICovx[2,1,] <- ICovx[1,2,]*TEScale
  ICovx[1,3,] <- ICovx[3,1,] <- ICovx[1,3,]/dataScale
  ICovx[1,4,] <- ICovx[4,1,] <- ICovx[1,4,]*TEScale
  ICovx[1,5,] <- ICovx[5,1,] <- ICovx[1,5,]/dataScale
  ICovx[2,2,] <- ICovx[2,2,]*TEScale*TEScale
  ICovx[2,3,] <- ICovx[3,2,] <- ICovx[2,3,]/dataScale*TEScale
  ICovx[2,4,] <- ICovx[4,2,] <- ICovx[2,4,]*TEScale*TEScale
  ICovx[2,5,] <- ICovx[5,2,] <- ICovx[2,5,]/dataScale*TEScale
  ICovx[3,3,] <- ICovx[3,3,]/dataScale/dataScale
  ICovx[3,4,] <- ICovx[4,3,] <- ICovx[3,4,]/dataScale*TEScale
  ICovx[3,5,] <- ICovx[5,3,] <- ICovx[3,5,]/dataScale/dataScale
  ICovx[4,4,] <- ICovx[4,4,]*TEScale*TEScale
  ICovx[4,5,] <- ICovx[5,4,] <- ICovx[4,5,]/dataScale*TEScale
  ICovx[5,5,] <- ICovx[5,5,]/dataScale/dataScale
  dim(ICovx) <- c(5,5,dim(mask))
      dim(IRdata) <- dimdata

# Results are currently scaled by TEScale (R) and dataScale (S)
      z <- list(IRdata=IRdata, InvTimes=InvTimes, segm=segm, sigma=sigma, L=L,
                fx=fx,Rx=Rx/TEScale,Sx=Sx*dataScale,Sf=Sf*dataScale,
                Rf=Rf/TEScale,ICovx=ICovx,Convx=Convx,rsdx=rsdx,
                method=method,varest=varest)
      class(z) <- "IRmixed"
      z
}

estimateIRfull2 <- function(IRsolidobj, InvTimes, segm,
                             TEScale = 100,
                             dataScale = 1000,
                             method = c("NLR", "QL"),
                             sigma = NULL,
                             L = 1,
                             varest = c("RSS","data"),
                             verbose = TRUE,
                             lower=c(0,0,0,0,0),
                             upper=c(.95,2,2,2,2)){
  IRdata <- IRsolidobj$IRdata
  InvTimes <- IRsolidobj$InvTimes
  segm <- IRsolidobj$segm
  sigma <- IRsolidobj$sigma
  L <- IRsolidobj$L
  Sx <- IRsolidobj$Sx/dataScale
  Rx <- IRsolidobj$Rx*TEScale
  fx <- IRsolidobj$fx
  Sfluid <- IRsolidobj$Sf/dataScale
  Rfluid <- IRsolidobj$Rf*TEScale
  Sf <- array(Sfluid,dim(Sx))
  Rf <- array(Rfluid,dim(Sx))
  Sf[segm==1] <- Sx[segm==1]
  Rf[segm==1] <- Rx[segm==1]
  method <- IRsolidobj$method
  varest <- IRsolidobj$varest
  mask <- segm>1
  nvoxel <- sum(mask)
  ntimes <- length(InvTimes)
  InvTimes[InvTimes==Inf] <- 50*max(InvTimes[InvTimes!=Inf])
  sind <- rep(1,ntimes)
  dimdata <- dim(IRdata)
  if(dimdata[1]!=ntimes) stop("estimateIRsolid: incompatible length of InvTimes")
  if(any(dimdata[-1]!=dim(mask))) stop("estimateIRsolid: incompatible dimension of segm")
  InvTimesScaled <- InvTimes/TEScale
  ## create necessary arrays
  npar <- 5 # th1 for f, th2 for R_s, th3 for S_s, , th4 for R_f, th5 for S_f
  df <- length(InvTimes)-npar
  rsdx <- array(0,dim(mask))
  ICovx <- array(0,c(5,5,prod(dim(mask))))
  Convx <- array(0,dim(mask))
  Convx[segm==1] <- 1
  # set ICovx for fluid as (numerically) diag(rep(Inf),3)
  ICovx[1,1,segm==1] <- 1e20
  ICovx[2,2,segm==1] <- 1e20
  ICovx[3,3,segm==1] <- 1e20
  ICovx[4,4,segm==1] <- 1e20
  ICovx[5,5,segm==1] <- 1e20
  isConv <- array(FALSE, nvoxel)
  isThresh <- array(FALSE, nvoxel)
  modelCoeff <- array(0, c(npar, nvoxel))
  invCov <- array(0, c(npar, npar, nvoxel))
  rsigma <- array(0, nvoxel)
  if (method[1] == "QL") {
    sig <- sigma/dataScale
    CL <- sig * sqrt(pi/2) * gamma(L + 0.5)/gamma(L)/gamma(1.5)
  }
  # initial parameters
  dim(IRdata) <- c(dimdata[1],prod(dim(segm)))
  IRdataSolid <- IRdata[,mask]
  thetas <- matrix(0,npar,nvoxel)
  thetas[5,] <- Sf[mask]
  thetas[4,] <- Rf[mask]
  thetas[3,] <- Sx[mask]
  thetas[2,] <- Rx[mask]
  thetas[1,] <- fx[mask]
  if (verbose){
    cat("Start estimation in", nvoxel, "voxel at", format(Sys.time()), "\n")
    pb <- txtProgressBar(0, nvoxel, style = 3)
  }
  for(xyz in 1:nvoxel){
    
    ivec <- IRdataSolid[, xyz]/dataScale
    th <- thetas[, xyz]
    ##
    ##   initialize using grid search and optim
    ##
    res <- if (method[1] == "NLR") try(optim(th, LSIRmix5, LSIRmix5grad, 
                                             Y=ivec, InvTimes=InvTimesScaled,
                                             method="L-BFGS-B",lower=lower,upper=upper))
    else try(optim(th, LSIRmix5QL, LSIRmix5QLgrad, 
                   Y=ivec, InvTimes=InvTimesScaled, 
                   CL = CL, sig = sig, L = L,
                   method="L-BFGS-B",lower=lower,upper=upper))
    
    if (!inherits(res, "try-error")){
      modelCoeff[,xyz] <- th <- res$par
      x <- attr(if(method[1]=="NLR") IRmix5(th,InvTimesScaled) 
                else IRmix5QL(th,InvTimesScaled,CL,sig,L),"grad") 
      rsigma[xyz] <- sqrt(res$value/df)
      isConv[xyz] <- res$convergence
      invCov[,,xyz] <- t(x)%*%x/res$value*df
    }
    if (verbose) if(xyz%/%1000*1000==xyz) setTxtProgressBar(pb, xyz)
  }
  if (verbose){
    close(pb)
    cat("Finished estimation", format(Sys.time()), "\n")
  }
  fx[mask] <- modelCoeff[1,]
  Rx[mask] <- modelCoeff[2,]
  Sx[mask] <- modelCoeff[3,]
  Rf[mask] <- modelCoeff[4,]
  Sf[mask] <- modelCoeff[5,]
  ICovx[,,mask] <- invCov
  Convx[mask] <- isConv
  rsdx[mask] <- rsigma
  ICovx[1,2,] <- ICovx[2,1,] <- ICovx[1,2,]*TEScale
  ICovx[1,3,] <- ICovx[3,1,] <- ICovx[1,3,]/dataScale
  ICovx[1,4,] <- ICovx[4,1,] <- ICovx[1,4,]*TEScale
  ICovx[1,5,] <- ICovx[5,1,] <- ICovx[1,5,]/dataScale
  ICovx[2,2,] <- ICovx[2,2,]*TEScale*TEScale
  ICovx[2,3,] <- ICovx[3,2,] <- ICovx[2,3,]/dataScale*TEScale
  ICovx[2,4,] <- ICovx[4,2,] <- ICovx[2,4,]*TEScale*TEScale
  ICovx[2,5,] <- ICovx[5,2,] <- ICovx[2,5,]/dataScale*TEScale
  ICovx[3,3,] <- ICovx[3,3,]/dataScale/dataScale
  ICovx[3,4,] <- ICovx[4,3,] <- ICovx[3,4,]/dataScale*TEScale
  ICovx[3,5,] <- ICovx[5,3,] <- ICovx[3,5,]/dataScale/dataScale
  ICovx[4,4,] <- ICovx[4,4,]*TEScale*TEScale
  ICovx[4,5,] <- ICovx[5,4,] <- ICovx[4,5,]/dataScale*TEScale
  ICovx[5,5,] <- ICovx[5,5,]/dataScale/dataScale
  dim(ICovx) <- c(5,5,dim(mask))
  dim(IRdata) <- dimdata
  
  # Results are currently scaled by TEScale (R) and dataScale (S)
  z <- list(IRdata=IRdata, InvTimes=InvTimes, segm=segm, sigma=sigma, L=L,
            fx=fx,Rx=Rx/TEScale,Sx=Sx*dataScale,Sf=Sf*dataScale,
            Rf=Rf/TEScale,ICovx=ICovx,Convx=Convx,rsdx=rsdx,
            method=method,varest=varest)
  class(z) <- "IRmixed"
  z
}


estimateIRsolidfixed <- function(IRmixedobj, TEScale = 100,
                                 dataScale = 1000,
                                 verbose = TRUE,
                                 lower=c(0.0),
                                 upper=c(0.95)){
   IRdata <- IRmixedobj$IRdata
   InvTimes <- IRmixedobj$InvTimes
   segm <- IRmixedobj$segm
   sigma <- IRmixedobj$sigma
   L <- IRmixedobj$L
   Sfluid <- IRmixedobj$Sf/dataScale
   Rfluid <- IRmixedobj$Rf*TEScale
   Ssolid <- IRmixedobj$Sx/dataScale
   Rsolid <- IRmixedobj$Rx*TEScale
   method <- IRmixedobj$method
   varest <- IRmixedobj$varest
   mask <- segm>1
   nvoxel <- sum(mask)
   ntimes <- length(InvTimes)
   df <- ntimes-1
   InvTimes[InvTimes==Inf] <- 50*max(InvTimes[InvTimes!=Inf])
   sind <- rep(1,ntimes)
   dimdata <- dim(IRdata)
   if(dimdata[1]!=ntimes) stop("estimateIRsolid: incompatible length of InvTimes")
   if(any(dimdata[-1]!=dim(mask))) stop("estimateIRsolid: incompatible dimension of segm")
   InvTimesScaled <- InvTimes/TEScale
   ## create necessary arrays
   npar <- 1 # th1 for f
   fx  <- rsdx <- array(0,dim(mask))
   ICovx <- array(0,dim(mask))
   Convx <- array(0,dim(mask))
   fx[segm==1] <- 1
   Rx <- Rsolid
   Sx <- Ssolid
   Convx[segm==1] <- 0
   # set ICovx for fluid as (numerically) diag(rep(Inf),3)
   ICovx[segm==1] <- 1e20
   isConv <- array(FALSE, nvoxel)
   isThresh <- array(FALSE, nvoxel)
   modelCoeff <- numeric(nvoxel)
   invCov <- numeric(nvoxel)
   rsigma <- numeric(nvoxel)
   if (method[1] == "QL") {
      sig <- sigma/dataScale
      CL <- sig * sqrt(pi/2) * gamma(L + 0.5)/gamma(L)/gamma(1.5)
   }
   # initial parametersIRdata=IRdata, InvTimes=InvTimes, segm=segm, sigma=sigma, L=L,
   dim(IRdata) <- c(dimdata[1],prod(dim(segm)))
   IRdataSolid <- IRdata[,mask]
   Rsm <- Rsolid[mask]
   Ssm <- Ssolid[mask]
   thetas <- rep(0.3,nvoxel)
   if (verbose){
      cat("Start estimation in", nvoxel, "voxel at", format(Sys.time()), "\n")
      pb <- txtProgressBar(0, nvoxel, style = 3)
   }
   for(xyz in 1:nvoxel){
      
      ivec <- IRdataSolid[, xyz]/dataScale
      th <- thetas[xyz]
      Rs <- Rsm[xyz]
      Ss <- Ssm[xyz]
      
      res <- if (method[1] == "NLR") try(nls(ivec ~ IRmix2fix(par, ITS, Sf, Ss, Rf, Rs),
                                          data = list(ITS=InvTimesScaled, Sf=Sfluid, Ss=Ss, Rf=Rfluid, Rs=Rs),
                                          start = list(par = th),
                                          control = list(maxiter = 200,
                                                         warnOnly = TRUE)),silent=TRUE)
      else try(nls(ivec ~ IRmix2fixQL(par, ITS, Sf, Ss, Rf, Rs, CL, sig, L),
                   data = list(ITS=InvTimesScaled, Sf=Sfluid, Ss=Ss, Rf=Rfluid, Rs=Rs,
                               CL = CL, sig = sig, L = L),
                   start = list(par = th),
                   control = list(maxiter = 200,
                                  warnOnly = TRUE)),silent=TRUE)
      if (inherits(res, "try-error")){
         # retry with port algorithm and bounds
         th <- pmin(upper,pmax(lower,th))
         res <- if (method[1] == "NLR") try(nls(ivec ~ IRmix2fix(par, ITS, Sf, Ss, Rf, Rs),
                                             data = list(ITS=InvTimesScaled, Sf=Sfluid, Ss=Ss, Rf=Rfluid, Rs=Rs),                                             start = list(par = th),
                                             algorithm="port",
                                             control = list(maxiter = 200,
                                                            warnOnly = TRUE),
                                             lower=lower, upper=upper),silent=TRUE)
         else try(nls(ivec ~ IRmix2fixQL(par, ITS, Sf, Ss, Rf, Rs, CL, sig, L),
                      data = list(ITS=InvTimesScaled, Sf=Sfluid, Ss=Ss, Rf=Rfluid, Rs=Rs,
                                  CL = CL, sig = sig, L = L),
                      start = list(par = th),
                      algorithm="port",
                      control = list(maxiter = 200,
                                     warnOnly = TRUE),
                      lower=lower, upper=upper),silent=TRUE)
      }
      if (!inherits(res, "try-error")) {
         sres <- if(varest[1]=="RSS") getnlspars(res) else
            getnlspars2(res, sigma, sind )
         isConv[xyz] <- as.integer(res$convInfo$isConv)
         modelCoeff[xyz] <- sres$coefficients
         if (sres$sigma != 0) {
            invCov[ xyz] <- sres$invCov*df
            rsigma[xyz] <- sres$sigma
         }
      }
      if (verbose) if(xyz%/%1000*1000==xyz) setTxtProgressBar(pb, xyz)
   }
   if (verbose){
      close(pb)
      cat("Finished estimation", format(Sys.time()), "\n")
   }
fx[mask] <- pmin(upper,pmax(lower,modelCoeff))
ICovx[mask] <- invCov
Convx[mask] <- isConv
rsdx[mask] <- rsigma
dim(IRdata) <- dimdata
# Results are currently scaled by TEscale (R) and Datascale (S)
      z <- list(IRdata=IRdata, InvTimes=InvTimes, segm=segm, sigma=sigma, L=L,fx=fx,Rx=Rx/TEScale,Sx=Sx*dataScale,Sf=Sfluid*dataScale,Rf=Rfluid/TEScale,ICovx=ICovx,Convx=Convx,rsdx=rsdx,method=method,varest=varest)
      class(z) <- "IRmixed"
      z
}

smoothIRSolid <- function(IRmixedobj, kstar=24, patchsize=1, alpha=0.025, mscbw=5,
      bysegment=TRUE, partial=TRUE, verbose=TRUE){
   segm <- IRmixedobj$segm
   mask <- segm>1
   nvoxel <- sum(mask)
   nv <- 3
   nFiles <- length(IRmixedobj$InvTimes)
   ICovx <- IRmixedobj$ICovx
   rsdx <- IRmixedobj$rsdx
   rsdhat <- medianFilter3D(rsdx,mscbw,mask)
   rsdhat[!mask] <- mean(rsdhat[mask])
   ICovx <- sweep(ICovx,3:5,rsdx/rsdhat,"*")
   dim(ICovx) <- c(3,3,prod(dim(mask)))
   lambda <- 2 * nv * qf(1 - alpha, nv, nFiles - nv)*
     switch(patchsize+1,1,2.77,3.46)
   if(verbose) cat("using lambda=", lambda, " patchsize=", patchsize,"\n")
   bi <- array(0,dim(mask))
   if(partial){
      ind <- (1:dim(ICovx)[3])[mask]
      ICovx2 <- ICovx[-1,-1,]
      for(i in ind) ICovx2[,,i] <- solve(solve(ICovx[,,i])[-1,-1])
      if(bysegment){
         bpars <- array(0,c(2,sum(segm==2)))
         bpars[1,] <- IRmixedobj$Rx[segm==2]
         bpars[2,] <- IRmixedobj$Sx[segm==2]
         icovbpars <- ICovx2[,,segm==2]
         z <- vpawscov2(bpars, kstar, icovbpars, segm==2, lambda=lambda,
                        patchsize=patchsize,verbose=verbose)
         IRmixedobj$Rx[segm==2] <- z$theta[1,]
         IRmixedobj$Sx[segm==2] <- z$theta[2,]
         bi[segm==2] <- z$bi
         bpars <- array(0,c(2,sum(segm==3)))
         bpars[1,] <- IRmixedobj$Rx[segm==3]
         bpars[2,] <- IRmixedobj$Sx[segm==3]
         icovbpars <- ICovx2[,,segm==3]
         z <- vpawscov2(bpars, kstar, icovbpars, segm==3, lambda=lambda,
                        patchsize=patchsize,verbose=verbose)
         IRmixedobj$Rx[segm==3] <- z$theta[1,]
         IRmixedobj$Sx[segm==3] <- z$theta[2,]
         bi[segm==3] <- z$bi
      } else {
         bpars <- array(0,c(2,nvoxel))
         bpars[1,] <- IRmixedobj$Rx[mask]
         bpars[2,] <- IRmixedobj$Sx[mask]
         icovbpars <- ICovx2[,,mask]
         z <- vpawscov2(bpars, kstar, icovbpars, mask, lambda=lambda,
                        patchsize=patchsize,verbose=verbose)
         IRmixedobj$Rx[mask] <- z$theta[1,]
         IRmixedobj$Sx[mask] <- z$theta[2,]
         bi[mask] <- z$bi
      } 
   }  else {
     if(bysegment){
       bpars <- array(0,c(3,sum(segm==2)))
       bpars[1,] <- IRmixedobj$fx[segm==2]
       bpars[2,] <- IRmixedobj$Rx[segm==2]
       bpars[3,] <- IRmixedobj$Sx[segm==2]
       icovbpars <- ICovx[,,segm==2]
       z <- vpawscov2(bpars, kstar, icovbpars, segm==2, lambda=lambda,
                      patchsize=patchsize,verbose=verbose)
       IRmixedobj$fx[segm==2] <- z$theta[1,]
       IRmixedobj$Rx[segm==2] <- z$theta[2,]
       IRmixedobj$Sx[segm==2] <- z$theta[3,]
       bi[segm==2] <- z$bi
       bpars <- array(0,c(3,sum(segm==3)))
       bpars[1,] <- IRmixedobj$fx[segm==3]
       bpars[2,] <- IRmixedobj$Rx[segm==3]
       bpars[3,] <- IRmixedobj$Sx[segm==3]
       icovbpars <- ICovx[,,segm==3]
       z <- vpawscov2(bpars, kstar, icovbpars, segm==3, lambda=lambda,
                      patchsize=patchsize,verbose=verbose)
       IRmixedobj$fx[segm==3] <- z$theta[1,]
       IRmixedobj$Rx[segm==3] <- z$theta[2,]
       IRmixedobj$Sx[segm==3] <- z$theta[3,]
       bi[segm==3] <- z$bi
     } else {
       bpars <- array(0,c(3,nvoxel))
       bpars[1,] <- IRmixedobj$fx[mask]
       bpars[2,] <- IRmixedobj$Rx[mask]
       bpars[3,] <- IRmixedobj$Sx[mask]
       icovbpars <- ICovx[,,mask]
       z <- vpawscov2(bpars, kstar, icovbpars, mask, lambda=lambda,
                      patchsize=patchsize,verbose=verbose)
       IRmixedobj$fx[mask] <- z$theta[1,]
       IRmixedobj$Rx[mask] <- z$theta[2,]
       IRmixedobj$Sx[mask] <- z$theta[3,]
       bi[mask] <- z$bi
     } 
   } 
   IRmixedobj$bi <- bi
   IRmixedobj$smoothPar <- c(z$lambda, z$hakt, alpha, patchsize, mscbw)
   IRmixedobj
}

estimateIR <- function(IRdataobj,
                       TEScale = 100,
                       dataScale = 1000,
                       method = c("NLR", "QL"),
                       varest = c("RSS","data"),
                       fixed = TRUE,
                       smoothMethod=c("PAWS","Depth"),
                       kstar = 24,
                       alpha = .025,
                       bysegment = TRUE,
                       verbose = TRUE){
  
   ergsFluid <- estimateIRfluid(IRdataobj, TEScale=TEScale,
   dataScale=dataScale, method=method, varest=varest)
   ergsBrain <- estimateIRsolid(ergsFluid, TEScale=TEScale, dataScale=dataScale)
   if(fixed) {
      if(smoothMethod[1]=="Depth") stop("not yet implemented")
# ergsSmooth <- SdepthSmooth(ergsBrain, segm)
      if(smoothMethod[1]=="PAWS") ergsBrain <- smoothIRSolid(ergsBrain, kstar, alpha=alpha, bysegment=bysegment)
   }
   ergsBrain <- estimateIRsolidfixed(ergsBrain, TEScale=TEScale, dataScale=dataScale)
   ergsBrain
}

MREdisplacement <- function(MagnFiles1, PhaseFiles1, MagnFiles2, PhaseFiles2, 
                            TI2=2400, IRmixobj, method=c("full","approx"),
                            rescale=FALSE,verbose=FALSE){
  segm <- IRmixobj$segm
  sdim <- dim(segm)
  nfiles <- length(MagnFiles1)
  if(length(PhaseFiles1)!=nfiles||length(MagnFiles2)!=nfiles||length(PhaseFiles2)!=nfiles){
    stop("Incompatible lengths of filelists")
  }
  imgdim1 <- readNIfTI(MagnFiles1[1], read_data=FALSE)@dim_[2:4]
  imgdim2 <- readNIfTI(MagnFiles2[1], read_data=FALSE)@dim_[2:4]
  imgdim3 <- readNIfTI(PhaseFiles1[1], read_data=FALSE)@dim_[2:4]
  imgdim4 <- readNIfTI(PhaseFiles2[1], read_data=FALSE)@dim_[2:4]
  if(any(imgdim1!=sdim)||any(imgdim2!=sdim)||any(imgdim3!=sdim)||any(imgdim4!=sdim)){
    stop("Incompatible image dimensions")
  }
  Mimg1 <- Mimg2 <- phiimg1 <- phiimg2 <- array(0,c(sdim,nfiles))
  for(i in 1:nfiles){
    Mimg1[,,,i] <- readNIfTI(MagnFiles1[i],reorient=FALSE)@.Data
    phiimg1[,,,i] <- readNIfTI(PhaseFiles1[i],reorient=FALSE,rescale_data=FALSE)@.Data
    Mimg2[,,,i] <- readNIfTI(MagnFiles2[i],reorient=FALSE)@.Data
    phiimg2[,,,i] <- readNIfTI(PhaseFiles2[i],reorient=FALSE,rescale_data=FALSE)@.Data
    rngimg <- range(phiimg1,phiimg2)
    if(verbose)  cat("range of phase images",rngimg,"\n")
    if(rescale) phiimg1 <- phiimg1/max(abs(rngimg))*pi
    if(rescale) phiimg2 <- phiimg2/max(abs(rngimg))*pi
  }
  Rf <- IRmixobj$Rf
  R1x <- IRmixobj$Rx
  mask <- segm>0
  CCs <- array(1-2*exp(-TI2*R1x),dim(Mimg1))
  CCf <- array(1-2*exp(-TI2*Rf),dim(Mimg1))
  if("full" %in% method){
    ss <- CCf*Mimg1*sin(phiimg1)-Mimg2*sin(phiimg2)
    cs <- CCf*Mimg1*cos(phiimg1)-Mimg2*cos(phiimg2)
  } else {
    ss <- -Mimg2*sin(phiimg2)
    cs <- -Mimg2*cos(phiimg2)
  }
  sf <- -CCs*Mimg1*sin(phiimg1)+Mimg2*sin(phiimg2)
  cf <- -CCs*Mimg1*cos(phiimg1)+Mimg2*cos(phiimg2)
  phis <- atan2(cs,ss)
  phif <- atan2(cf,sf)
  phis[!mask] <- 0
  phif[!mask] <- 0
  z <- list(phisolid=phis, phifluid=phif)
  class(z) <- "IRMREbiphasic"
  z
}


