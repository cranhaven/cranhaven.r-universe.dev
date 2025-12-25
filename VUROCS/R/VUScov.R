#' @title Covariance of two volumes under the ROC surface
#' @description Computes the covariance of the two volumes under the ROC surface (VUS) implied by two predictions \code{fx1} and \code{fx2} (i.e. values of two ranking functions f1 and f2) for a vector of realisations \code{y} (i.e. realised categories) in a multi-class classification problem.
#' @param y a vector of realized categories.
#' @param fx1 a vector of predicted values of the ranking function f1.
#' @param fx2 a vector of predicted values of the ranking function f2.
#' @param ncores number of cores to be used for parallelized computations. Its default value is 1.
#' @param clusterType type of cluster to be initialized in case more than one core is used for calculations. Its default value is "SOCK". For details regarding the different types to be used, see \code{\link[parallel]{makeCluster}}.
#' @return The implemented algorithm is based on Waegeman, De Baets and Boullart (2008). A list of length three is returned, containing the following components:
#' \item{cov}{covariance of the two volumes under the ROC surface implied by f1 and f2}
#' \item{val_f1}{volume under the ROC surface implied by f1}
#' \item{val_f2}{volume under the ROC surface implied by f2}
#' @examples VUScov(c(1,2,1,3,2,3),c(1,2,3,4,5,6),c(1,3,2,4,6,5))
#' @references Waegeman W., De Baets B., Boullart L., 2008. On the scalability of ordered multi-class ROC analysis. Computational Statistics & Data Analysis 52, 3371-3388.

VUScov<-function(y,fx1,fx2,ncores=1,clusterType="SOCK"){

  if (any(is.na(fx1)) | any(is.na(fx2)) | any(is.na(y))) {
    stop("\n no NA values allowed for neither 'fx1', 'fx2' nor 'y'")
  }
  
  if (length(y)!=length(fx1) | length(y)!=length(fx2)) {
  	stop("\n 'fx1', 'fx2' and 'y' need to have the same dimension.")
  }

  dat <- cbind(y,fx1,fx2)
  n   <- length(y)
  u   <- sort(unique(y))
  r   <- length(u)
  uu  <- 1:r
  dat <- cbind(dat,rep(NA,n))

  for (i in 1:n) {
    dat[i,4] <- which(dat[i,1]==u)
  }

  part <- matrix(0, nrow=2^r, ncol=r)
  for (i in 1:r) {
    part[,i] <- kronecker((0:(2^(r-i+1)-1))%%2, matrix(1,nrow=2^(i-1),ncol=1))
  }


  foreachPartition <- function(s,vurocs1,vurocs2,uu,dat,part,r) {
  
    W <- vector(mode="list",length=r)
    for (k in 1:r) {
      Z11 <- uu[as.logical(part[s,])]
      isinK <- dat[,1]==u[k]
      if (sum(uu[k]==Z11)>0) {
        for (i in 1:sum(isinK)) {
          W[[k]] <- c(W[[k]],list(matrix(rep(dat[isinK,,drop=FALSE][i,],2),ncol=4, byrow=TRUE)))
        }
      }
      else {
        for (l in 1:sum(isinK)) {
          for (j in 1:sum(isinK)) {
            W[[k]] <- c(W[[k]],list(matrix(c(dat[isinK,,drop=FALSE][l,],dat[isinK,,drop=FALSE][j,]),ncol=4, byrow=TRUE)))
          }
        }
      }
    }

    len <- integer(r-1)
    for (k in 1:(r-1)) {
      len[k] <- length(W[[k]])*length(W[[k+1]])
    }

    len <- c(0,cumsum(len))
    E <- integer(len[length(len)])
    for (k in 1:(r-1)) {
      out1 <- 0
      for (a in 1:length(W[[k]])) {
        out1 <- out1+1
        out2 <- 0
        for (b in 1:length(W[[k+1]])) {
          out2 <- out2+1
          if (W[[k]][[a]][1,2] < W[[k+1]][[b]][1,2] & W[[k]][[a]][2,3] < W[[k+1]][[b]][2,3]) {
            E[out2+(out1-1)*length(W[[k+1]])+len[k]] <- 1
          }
          else {
            E[out2+(out1-1)*length(W[[k+1]])+len[k]] <- 0
          }
        }
      }
    }

    node <- c(0,cumsum(sapply(W,length)))
    gamma <- integer(node[length(node)])
    gamma[1:length(W[[1]])] <- 1
    for (k in 1:(r-1)) {
      int1 <- 0
      for (a in 1:length(W[[k]])) {
        int1 <- int1 + 1
        int2 <- 0
        for (b in 1:length(W[[k+1]])) {
          int2 <- int2 + 1
          gamma[int2+node[k+1]] <- gamma[int2+node[k+1]]+gamma[int1+node[k]]*E[int2+(int1-1)*length(W[[k+1]])+len[k]]
        }
      }
    }


    ped <- gamma[((length(gamma)-length(W[[r]]))+1):length(gamma)]
    paths <- sum(ped)
    Z2 <- uu[!as.logical(part[s,])]
    n.under <- prod(vurocs1[[2]][Z2])*prod(vurocs1[[2]])
    cov <- prod(vurocs1[[2]][Z2]-1)*(paths/n.under- (vurocs1[[1]]*vurocs2[[1]]))
  }
  vurocs1 <- VUS(y,fx1)
  vurocs2 <- VUS(y,fx2)
  
  if (ncores==1) {
    cov <- sapply(1:2^r,foreachPartition,vurocs1,vurocs2,uu,dat,part,r)
  } else {
    cl <- makeCluster(min(ncores,detectCores()-1), type=clusterType )
    registerDoParallel(cl)
    cov <- foreach(i=1:2^r,.combine = c) %dopar% foreachPartition(i,vurocs1,vurocs2,uu,dat,part,r)
    on.exit(stopCluster(cl))
  }
  
  cov <- sum(cov)/prod(vurocs1$count)
  
  return(list(cov=cov,val_f1=vurocs1[[1]],val_f2=vurocs2[[1]]))
}