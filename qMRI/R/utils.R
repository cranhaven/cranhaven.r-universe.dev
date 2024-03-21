setOldClass(c("MPMData","ESTATICSModel","sESTATICSModel","qMaps"))

extract.default <- function(x, ...) awsMethods::extract(x)
extract.MPMData <- function(x,what, ...){
  class(x) <- "list"
  onames <- names(x)
  select <- what[what %in% onames]
  
  # return single component or list with selected components
  z <- x[select]
  sdim <- x$sdim
  mask <- x$mask
  if(any(dim(x$ddata)[-1]!=sdim)){
    #      data are reduced to only contain voxel within mask, need to expand
    if("ddata" %in% select){
      ddata <- array(0,c(x$nFiles, prod(sdim)))
      ddata[, mask] <- x$ddata
      dim(ddata) <- c(x$nFiles, sdim)
      z[["ddata"]] <- ddata
    }
  }
  invisible(if(length(select)==1) z[[select]] else z[select])
}

extract.ESTATICSModel <- function(x,what, ...){
  class(x) <- "list"
  onames <- names(x)
  select <- what[what %in% onames]
  z <- x[select]
  sdim <- x$sdim
  mask <- x$mask
  if(any(dim(x$modelCoeff)[-1]!=sdim)){
    #      data are reduced to only contain voxel within mask, need to expand
    if("modelCoeff" %in% select){
      modelCoeff <- array(0,c(x$model+2, prod(sdim)))
      modelCoeff[, mask] <- x$modelCoeff
      dim(modelCoeff) <- c(x$model+2, sdim)
      z[["modelCoeff"]] <- modelCoeff
    }
    if("invCov" %in% select){
      invCov <- array(0,c(x$model+2, x$model+2, prod(sdim)))
      invCov[, , mask] <- x$invCov
      dim(invCov) <- c(x$model+2, x$model+2, sdim)
      z[["invCov"]] <- invCov
    }
    if("isThresh" %in% select){
      isThresh <- array(0,prod(sdim))
      isThresh[mask] <- x$isThresh
      dim(isThresh) <- sdim
      z[["isThresh"]] <- isThresh
    }
    if("isConv" %in% select){
      isConv <- array(0,prod(sdim))
      isConv[mask] <- x$isConv
      dim(isConv) <- sdim
      z[["isConv"]] <- isConv
    }
    if("sigma" %in% select){
      sigma <- array(0,prod(sdim))
      sigma[mask] <- x$sigma
      dim(sigma) <- sdim
      z[["sigma"]] <- sigma
    }
    if("rsigma" %in% select){
      sigma <- array(0,prod(sdim))
      sigma[mask] <- x$rsigma
      dim(sigma) <- sdim
      z[["rsigma"]] <- sigma
    }
  }
  invisible(if(length(select)==1) z[[select]] else z[select])
}

extract.sESTATICSModel <- function(x,what, ...){
  class(x) <- "list"
  onames <- names(x)
  select <- what[what %in% onames]
  z <- x[select]
  sdim <- x$sdim
  mask <- x$mask
  if(any(dim(x$modelCoeff)[-1]!=sdim)){
    #      data are reduced to only contain voxel within mask, need to expand
    if("modelCoeff" %in% select){
      modelCoeff <- array(0,c(x$model+2, prod(sdim)))
      modelCoeff[, mask] <- x$modelCoeff
      dim(modelCoeff) <- c(x$model+2, sdim)
      z[["modelCoeff"]] <- modelCoeff
    }
    if("invCov" %in% select){
      invCov <- array(0,c(x$model+2, x$model+2, prod(sdim)))
      invCov[, , mask] <- x$invCov
      dim(invCov) <- c(x$model+2, x$model+2, sdim)
      z[["invCov"]] <- invCov
    }
    if("isThresh" %in% select){
      isThresh <- array(0,prod(sdim))
      isThresh[mask] <- x$isThresh
      dim(isThresh) <- sdim
      z[["isThresh"]] <- isThresh
    }
    if("isConv" %in% select){
      isConv <- array(0,prod(sdim))
      isConv[mask] <- x$isConv
      dim(isConv) <- sdim
      z[["isConv"]] <- isConv
    }
    if("sigma" %in% select){
      sigma <- array(0,prod(sdim))
      sigma[mask] <- x$sigma
      dim(sigma) <- sdim
      z[["sigma"]] <- sigma
    }
    if("rsigma" %in% select){
      sigma <- array(0,prod(sdim))
      sigma[mask] <- x$rsigma
      dim(sigma) <- sdim
      z[["rsigma"]] <- sigma
    }
    if("bi" %in% select){
      bi <- array(0,prod(sdim))
      bi[mask] <- x$bi
      dim(bi) <- sdim
      z[["bi"]] <- bi
    }
    if("smoothedData" %in% select){
      ddata <- array(0,c(dim(x$smoothedData)[1], prod(sdim)))
      ddata[, mask] <- x$smoothedData
      dim(ddata) <- c(dim(x$smoothedData)[1], sdim)
      z[["smoothedData"]] <- ddata
    }
  }
  invisible(if(length(select)==1) z[[select]] else z[select])
}

extract.qMaps <- function(x,what, ...){
  class(x) <- "list"
  onames <- names(x)
  select <- what[what %in% onames]
  z <- x[select]
  sdim <- x$sdim
  mask <- x$mask
  if(length(x$R1)==sum(mask)){
    #      data are reduced to only contain voxel within mask, need to expand
    if("b1Map" %in% select){
      b1Map <- array(0,prod(sdim))
      b1Map[mask] <- x$b1Map
      dim(b1Map) <- sdim
      z[["b1Map"]] <- b1Map
    }
    if("R1" %in% select){
      R1 <- array(0,prod(sdim))
      R1[mask] <- x$R1
      dim(R1) <- sdim
      z[["R1"]] <- R1
    }
    if("R2star" %in% select){
      R2star <- array(0,prod(sdim))
      R2star[mask] <- x$R2star
      dim(R2star) <- sdim
      z[["R2star"]] <- R2star
    }
    if("PD" %in% select){
      PD <- array(0,prod(sdim))
      PD[mask] <- x$PD
      dim(PD) <- sdim
      z[["PD"]] <- PD
    }
    if("MT" %in% select){
      MT <- array(0,prod(sdim))
      MT[mask] <- x$MT
      dim(MT) <- sdim
      z[["MT"]] <- MT
    }
  }
  invisible(if(length(select)==1) z[[select]] else z[select])
}

'[.MPMData' <- function(x, i, j, k, ...){
            args <- sys.call(-1)
            sdim <- x$sdim
            if (missing(i)) i <- 1:sdim[1]
            if (missing(j)) j <- 1:sdim[2]
            if (missing(k)) k <- 1:sdim[3]
            if (is.logical(i)) ddimi <- x$sdim[1] else ddimi <- length(i)
            if (is.logical(j)) ddimj <- x$sdim[2] else ddimj <- length(j)
            if (is.logical(k)) ddimk <- x$sdim[3] else ddimk <- length(k)

            mask <- x$mask
            nvoxel <- sum(mask)
            newmask <- mask[i,j,k,drop=FALSE]
            newnvoxel <- sum(newmask)
            if(newnvoxel != nvoxel){
              #  need to adjust index of voxel in mask
              index <- array(0,x$sdim)
              index[mask] <- 1:nvoxel
              index <- index[i,j,k]
              index <- index[index>0]
              x$ddata <- x$ddata[,index]
            }
            x$mask <- newmask
            x$sdim <- c(ddimi, ddimj, ddimk)
            if(is.null(x$indx)) x$indx <- i else indx <- x$indx[i]
            if(is.null(x$indy)) x$indy <- j else indy <- x$indy[j]
            if(is.null(x$indz)) x$indz <- k else indz <- x$indz[k]
            class(x) <- "MPMData"
            invisible(x)
          }

'[.ESTATICSModel' <- function(x, i, j, k, ...){
  args <- sys.call(-1)
  sdim <- x$sdim
  if (missing(i)) i <- 1:sdim[1]
  if (missing(j)) j <- 1:sdim[2]
  if (missing(k)) k <- 1:sdim[3]
  if (is.logical(i)) ddimi <- x$sdim[1] else ddimi <- length(i)
  if (is.logical(j)) ddimj <- x$sdim[2] else ddimj <- length(j)
  if (is.logical(k)) ddimk <- x$sdim[3] else ddimk <- length(k)
  
  mask <- x$mask
  nvoxel <- sum(mask)
  newmask <- mask[i,j,k,drop=FALSE]
  newnvoxel <- sum(newmask)
  if(newnvoxel != nvoxel){
    #  need to adjust index of voxel in mask
    index <- array(0,x$sdim)
    index[mask] <- 1:nvoxel
    index <- index[i,j,k]
    index <- index[index>0]
    x$modelCoeff <- x$modelCoeff[,index]
    x$invCov <- x$invCov[,,index]
    x$isThresh <- x$isThresh[index]
    x$isConv <- x$isConv[index]
    x$rsigma <- x$rsigma[index]
    if(!is.null(x$shat)) x$shat <- x$shat[,index]
    if(!is.null(x$sigma)) x$sigma <- x$sigma[index]
  }
  x$mask <- newmask
  x$sdim <- c(ddimi, ddimj, ddimk)
  if(is.null(x$indx)) x$indx <- i else indx <- x$indx[i]
  if(is.null(x$indy)) x$indy <- j else indy <- x$indy[j]
  if(is.null(x$indz)) x$indz <- k else indz <- x$indz[k]
  class(x) <- "ESTATICSModel"
  invisible(x)
}

'[.sESTATICSModel' <- function(x, i, j, k, ...){
  args <- sys.call(-1)
  sdim <- x$sdim
  if (missing(i)) i <- 1:sdim[1]
  if (missing(j)) j <- 1:sdim[2]
  if (missing(k)) k <- 1:sdim[3]
  if (is.logical(i)) ddimi <- x$sdim[1] else ddimi <- length(i)
  if (is.logical(j)) ddimj <- x$sdim[2] else ddimj <- length(j)
  if (is.logical(k)) ddimk <- x$sdim[3] else ddimk <- length(k)
  
  mask <- x$mask
  nvoxel <- sum(mask)
  newmask <- mask[i,j,k,drop=FALSE]
  newnvoxel <- sum(newmask)
  if(newnvoxel != nvoxel){
    #  need to adjust index of voxel in mask
    index <- array(0,x$sdim)
    index[mask] <- 1:nvoxel
    index <- index[i,j,k]
    index <- index[index>0]
    x$modelCoeff <- x$modelCoeff[,index]
    x$invCov <- x$invCov[,,index]
    x$isThresh <- x$isThresh[index]
    x$isConv <- x$isConv[index]
    x$rsigma <- x$rsigma[index]
    if(!is.null(x$shat)) x$shat <- x$shat[,index]
    if(!is.null(x$sigma)) x$sigma <- x$sigma[index]
    x$bi <- x$bi[index]
    if(!is.null(x$smoothedData)) x$smoothedData <- x$smoothedData[,index]
  }
  x$mask <- newmask
  x$sdim <- c(ddimi, ddimj, ddimk)
  if(is.null(x$indx)) x$indx <- i else indx <- x$indx[i]
  if(is.null(x$indy)) x$indy <- j else indy <- x$indy[j]
  if(is.null(x$indz)) x$indz <- k else indz <- x$indz[k]
  class(x) <- "sESTATICSModel"
  invisible(x)
}

'[.qMaps' <- function(x, i, j, k, ...){
  args <- sys.call(-1)
  sdim <- x$sdim
  if (missing(i)) i <- 1:sdim[1]
  if (missing(j)) j <- 1:sdim[2]
  if (missing(k)) k <- 1:sdim[3]
  if (is.logical(i)) ddimi <- x$sdim[1] else ddimi <- length(i)
  if (is.logical(j)) ddimj <- x$sdim[2] else ddimj <- length(j)
  if (is.logical(k)) ddimk <- x$sdim[3] else ddimk <- length(k)
  
  mask <- x$mask
  nvoxel <- sum(mask)
  newmask <- mask[i,j,k,drop=FALSE]
  newnvoxel <- sum(newmask)
  if(newnvoxel != nvoxel){
    #  need to adjust index of voxel in mask
    index <- array(0,x$sdim)
    index[mask] <- 1:nvoxel
    index <- index[i,j,k]
    index <- index[index>0]
    x$b1Map <- x$b1Map[,index]
    x$R1 <- x$R1[index]
    x$R2star <- x$R2star[index]
    x$PD <- x$PD[index]
    if(!is.null(x$MT)) x$MT <- x$MT[index]
    }
  x$mask <- newmask
  x$sdim <- c(ddimi, ddimj, ddimk)
  if(is.null(x$indx)) x$indx <- i else indx <- x$indx[i]
  if(is.null(x$indy)) x$indy <- j else indy <- x$indy[j]
  if(is.null(x$indz)) x$indz <- k else indz <- x$indz[k]
  class(x) <- "qMaps"
  invisible(x)
}
