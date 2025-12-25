#' Generate Cutting Points for a Multidimensional Matrix
#'
#' For a given column \code{cur.ch} that belongs to a matrix, and a given number of cuts \code{n},
#' compute \code{n-1} bins using either fixed of combined limits
#'
#' @param mat the matrix to cut
#' @param n the number of cuts to generate (defaults to 5)
#' @param count.lim the minimum number of counts to consider for density (defaults to 40)
#'
#' @return a list of of cuts for each column in \code{mat}, see \emph{details}
#'
#' @details the fixed limits correspond to 5 equally spaced values over the range of the column.
#'    the combined limits take the local minima and maxima determined using the \code{\link{localMinima}}
#'    and \code{\link{localMaxima}} functions, to adjust the limits using the following algorithm:
#'      \itemize{
#'        \item define \code{d} as half the distance between 2 fixed limits
#'        \item merge local minima and local maxima that are closer than \code{d}
#'        \item if any fixed limit is closer to a local minima than \code{d}, move the fixed limit to the local minima;
#'              move the limits that are not been moved yet, and that are before and after the moved limit
#'              so that they are evenly spread; repeat until no fixed limit can be moved
#'        \item if some limits have been moved to a local minima, \strong{remove} limits that are closer than \code{d} to
#'              a local maxima; move the limits that are not been moved yet, and that are before and after
#'              the deleted limit so that they are evenly spread; repeat until no fixed limit can be moved
#'        \item if no limits has been moved to a local minima, move limits that are closer than \code{d} to
#'              a local maxima; move the limits that are not been moved yet, and that are before and after
#'              the moved limit so that they are evenly spread; repeat until no fixed limit can be moved
#'      }
#'    The function returns a list of lists, one for each column in \code{mat}, consisting of
#'      \itemize{
#'        \item \code{cur.dens} the density used to describe the data
#'        \item \code{cur.hist} the histogram used to describe the data
#'        \item \code{fixed} the fixed, evenly spaced cuts
#'        \item \code{minima} the local minima detected in the data
#'        \item \code{maxima} the local maxima detected in the data
#'        \item \code{combined} the cuts defined using a combination of fixed positions, local minima and local maxima
#'      }
#'
#' @example examples/example.cut.R
#'
#' @author Yann Abraham
#' @importFrom stats density
#' @importFrom graphics hist
#' @export
make.cut <- function(mat,n=5,count.lim=40) {
  if(n<3) {
    stop('n should be greater than 2')
  }
  if(hilbert.order(mat)<n) {
      warning('n is much larger than the suggested number of cuts: ',hilbert.order(mat),' see ?hilbert.order')
  }
  final.cuts <- lapply(dimnames(mat)[[2]],function(cur.ch) {
    cat(cur.ch,'\n')
    x <- mat[,cur.ch]
    cur.dens <- density(x)
    cur.hist <- hist(x,plot=F)#,breaks=5*n)

    # define fixed breaks
    fch <- seq(min(x),max(x),length.out=n)
    ach <- fch

    # find local minima
    clean.dens <- cur.hist$density
    clean.dens[cur.hist$counts<count.lim] <- 0
    minch <- cur.hist$mids[localMinima(clean.dens)]
    maxch <- cur.hist$mids[localMaxima(clean.dens)]

    # find the average distance between fixed breakpoints
    d <- mean(diff(fch))/2

    # if 2 local minima are closer than d, merge them
    while(any(diff(minch)<d)) {
      # find the smallest difference
      i <- which.min(diff(minch))
      # replace the 1st one with the average of the 2 values
      minch[i] <- mean(minch[i:(i+1)])
      # remove the 2nd value
      minch <- minch[-(i+1)]
    }

    # if 2 local maxima are closer than d, merge them
    while(any(diff(maxch)<d)) {
      # find the smallest difference
      i <- which.min(diff(maxch))
      # replace the 1st one with the average of the 2 values
      maxch[i] <- mean(maxch[i:(i+1)])
      # remove the 2nd value
      maxch <- maxch[-(i+1)]
    }

    # is there any local minima close to a fixed breakpoint?
    # 'close' is defined as less than half the distance between
    # 2 fixed breakpoints
    moved <- rep(0,n)
    moved[c(1,n)] <- 1
    mfdiff <- lapply(minch,function(x) abs(fch[-c(1,n)]-x))
    mfdiff <- do.call('cbind',mfdiff)
    while(any(mfdiff<d)) {
      # for each mch, move the closest fch to the corresponding mch
      cur.closest <- which(!moved[-c(1,n)] & mfdiff==min(mfdiff),arr.ind=T)
      mclosest <- cur.closest[,2]
      aclosest <- cur.closest[,1]

      # move the breakpoint
      ach[1+aclosest] <- minch[mclosest]

      # adjust the other breakpoints
      z <- aclosest+1
      u <- z+min(which(moved[z:n]==1))-1
      l <- max(which(moved[1:z]==1))
      ach[z:u] <- seq(ach[z],ach[u],length.out=u-z+1)
      ach[l:z] <- seq(ach[l],ach[z],length.out=z-l+1)

      # set the position to moved
      moved[aclosest+1] <- 1

      # redefine mfdiff
      mfdiff <- lapply(minch,function(x) abs(ach[-c(1,n)]-x))
      mfdiff <- do.call('cbind',mfdiff)
      # reset positions that are already adjusted
      mfdiff[as.logical(moved[-c(1,n)]),] <- d
    }

    # Now depending on whether we found a local minima,
    # either remove any breakpoint that is close to a local maxima,
    # or move the breakpoint to the local maxima
    # first, is there any local maxima close to a fixed breakpoint
    # that has not been moved yet?
    mfdiff <- lapply(maxch,function(x) abs(ach[-c(1,n)]-x))
    mfdiff <- do.call('cbind',mfdiff)
    mfdiff[as.logical(moved[-c(1,n)]),] <- d

    if(any(mfdiff<d)) {
      if(any(moved[-c(1,n)]==1)) {
        while(any(mfdiff<d) & length(ach)>3) {
          # for each mch, move the closest fch to the corresponding mch
          cur.closest <- which(!moved[-c(1,n)] & mfdiff==min(mfdiff),arr.ind=T)
          aclosest <- cur.closest[,1]

          # remove the breakpoint
          ach <- ach[-(1+aclosest)]
          moved <- moved[-(1+aclosest)]

          # adjust the other breakpoints
          z <- aclosest
          u <- z+min(which(moved[(z+1):length(moved)]==1))
          ach[z:u] <- seq(ach[z],ach[u],length.out=u-z+1)

          # redefine mfdiff
          mfdiff <- lapply(maxch,function(x) abs(ach[-c(1,n)]-x))
          mfdiff <- do.call('cbind',mfdiff)
          # reset positions that are already adjusted
          mfdiff[as.logical(moved[-c(1,n)]),] <- d
        }
      } else {
        while(any(mfdiff<d)) {
          # for each mch, move the closest fch to the corresponding mch
          cur.closest <- which(!moved[-c(1,n)] & mfdiff==min(mfdiff),arr.ind=T)
          mclosest <- cur.closest[,2]
          aclosest <- cur.closest[,1]

          # move the breakpoint
          ach[1+aclosest] <- maxch[mclosest]

          # adjust the other breakpoints
          z <- aclosest+1
          u <- z+min(which(moved[z:n]==1))-1
          l <- max(which(moved[1:z]==1))
          ach[z:u] <- seq(ach[z],ach[u],length.out=u-z+1)
          ach[l:z] <- seq(ach[l],ach[z],length.out=z-l+1)

          # set the position to moved
          moved[aclosest+1] <- 1

          # redefine mfdiff
          mfdiff <- lapply(maxch,function(x) abs(ach[-c(1,n)]-x))
          mfdiff <- do.call('cbind',mfdiff)
          # reset positions that are already adjusted
          mfdiff[as.logical(moved[-c(1,n)]),] <- d
        }
      }
    }

    # final adjustment for positions that have not been moved
    while(any(moved==0)) {
      # move the unmoved limits so that they are equally spaced
      # find the 1st zero
      z <- min(which(moved==0))
      # find the 1 closest to the first 0 upward
      u <- z+min(which(moved[z:n]==1))-1
      # find the 1 closest to the first 0 downward
      l <- max(which(moved[1:z]==1))

      ach[l:u] <- seq(ach[l],ach[u],length.out=u-l+1)
      moved[u:l] <- 1
    }

    return(list(dens=cur.dens,
                hist=cur.hist,
                minima=minch,
                maxima=maxch,
                cuts=list(fixed=fch,
                          combined=ach)))
  })
  names(final.cuts) <- dimnames(mat)[[2]]
  return(final.cuts)
}
