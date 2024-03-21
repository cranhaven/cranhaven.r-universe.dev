setOldClass(c("IRdata","IRfluid","IRMixed"))

extract.IRdata <- function(x,what, ...){
  class(x) <- "list"
  onames <- names(x)
  select <- what[what %in% onames]

  # return single component or list with selected components
  z <- x[select]
  invisible(if(length(select)==1) z[[select]] else z[select])
}

extract.IRfluid <- function(x,what, ...){
  class(x) <- "list"
  onames <- names(x)
  select <- what[what %in% onames]
  z <- x[select]
  invisible(if(length(select)==1) z[[select]] else z[select])
}

extract.IRmixed <- function(x,what, ...){
  class(x) <- "list"
  onames <- names(x)
  select <- what[what %in% onames]
  z <- x[select]
  invisible(if(length(select)==1) z[[select]] else z[select])
}

'[.IRdata' <- function(x, i, j, k, tind, ...){
            args <- sys.call(-1)
            sdim <- dim(x$IRdata)
            if (missing(i)) i <- 1:sdim[2]
            if (missing(j)) j <- 1:sdim[3]
            if (missing(k)) k <- 1:sdim[4]
            if (missing(tind)) tind <- 1:sdim[1]
            if (is.logical(i)) ddimi <- x$sdim[1] else ddimi <- length(i)
            if (is.logical(j)) ddimj <- x$sdim[2] else ddimj <- length(j)
            if (is.logical(k)) ddimk <- x$sdim[3] else ddimk <- length(k)
            x$IRdata <- x$IRdata[tind, i, j, k]
            x$InvTimes <- x$InvTimes[tind]
            x$segm <- x$segm[i,j,k]
            x
            }

'[.IRfluid' <- function(x, i, j, k, ...){
            args <- sys.call(-1)
            sdim <- dim(x$IRdata)
            if (missing(i)) i <- 1:sdim[2]
            if (missing(j)) j <- 1:sdim[3]
            if (missing(k)) k <- 1:sdim[4]
            if (is.logical(i)) ddimi <- x$sdim[1] else ddimi <- length(i)
            if (is.logical(j)) ddimj <- x$sdim[2] else ddimj <- length(j)
            if (is.logical(k)) ddimk <- x$sdim[3] else ddimk <- length(k)
            x$IRdata <- x$IRdata[, i, j, k]
            x$segm <- x$segm[i,j,k]
            x$Sx <- x$Sx[i,j,k]
            x$Rx <- x$Rx[i,j,k]
            x$Convx <- x$Convx[i,j,k]
            x
            }

'[.IRmixed' <- function(x, i, j, k, ...){
            args <- sys.call(-1)
            sdim <- dim(x$IRdata)
            if (missing(i)) i <- 1:sdim[2]
            if (missing(j)) j <- 1:sdim[3]
            if (missing(k)) k <- 1:sdim[4]
            if (is.logical(i)) ddimi <- x$sdim[1] else ddimi <- length(i)
            if (is.logical(j)) ddimj <- x$sdim[2] else ddimj <- length(j)
            if (is.logical(k)) ddimk <- x$sdim[3] else ddimk <- length(k)
            x$IRdata <- x$IRdata[, i, j, k]
            x$segm <- x$segm[i,j,k]
            x$Sx <- x$Sx[i,j,k]
            x$Rx <- x$Rx[i,j,k]
            x$fx <- x$fx[i,j,k]
            x$Convx <- x$Convx[i,j,k]
            dICovx <- dim(x$ICovx)
            x$ICovx <- if(length(dICovx)==3) x$ICovx[i,j,k] else x$ICovx[,,i,j,k]
            x$rsdx <- x$rsdx[i,j,k]
            if(!is.null(x$bi)) x$bi <- x$bi[i,j,k]
            x
            }


