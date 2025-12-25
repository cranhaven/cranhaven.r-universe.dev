#' @title Variance of the volume under the ROC surface
#' @description Computes the volume under the ROC surface (VUS) and its variance for a vector of realisations \code{y} (i.e. realised categories) and a vector of predictions \code{fx} (i.e. values of the a ranking function f) for the purpose of assessing the discrimiatory power in a multi-class classification problem.
#' @param y a vector of realized categories.
#' @param fx a vector of predicted values of the ranking function f.
#' @param ncores number of cores to be used for parallelized computations. The default value is 1.
#' @param clusterType type of cluster to be initialized in case more than one core is used for calculations. The default values is "SOCK". For details regarding the different types to be used, see \code{\link[parallel]{makeCluster}}.
#' @return The implemented algorithm is based on Waegeman, De Baets and Boullart (2008). A list of length two is returned, containing the following components:
#' \item{var}{variance of the volume under the ROC surface}
#' \item{val}{volume under the ROC surface}
#' @examples VUSvar(rep(1:5,each=3),c(1,2,3,rep(2:5,each=3)))
#' @references Waegeman W., De Baets B., Boullart L., 2008. On the scalability of ordered multi-class ROC analysis. Computational Statistics & Data Analysis 52, 3371-3388.

 
VUSvar <- function(y,fx,ncores = 1, clusterType="SOCK") {

  if (any(is.na(fx)) | any(is.na(y))) {
    stop("\n no NA values allowed for neither 'fx' nor 'y'")
  }
  
  if(length(y)!=length(fx)){
  	stop("\n 'fx' and 'x' need to have the same dimension.")
  }

	n <- length(y)
	dat <- cbind(y,fx)
	dat <- dat[order(y,decreasing=TRUE), ]
	dat <- dat[order(dat[,2]), ]
	u <- sort(unique(y))
	r <- length(u)


	dat <- cbind(dat,rep(NA,n),seq(1,n))
	psi   <- rep(0,r)
	out <- rep(0,n)
	count<-rep(0,r)

	for (i in 1:n) {

	  cat <- which(dat[i,1] == u)
		dat[i,3] <- cat
		if (cat == 1) {
		  psi[1] <- psi[1]+1
		} else {
		  psi[cat] <- psi[cat]+psi[cat-1]
		}
		out[i] <- count[cat]
		count[cat] <- count[cat] + 1
	}
  vurocs=list(val=psi[r]/prod(count),count=count)
  
	part <- matrix(0, nrow=2^r, ncol=r)
	for (i in 1:r) {
		part[,i] <- kronecker((0:(2^(r-i+1)-1))%%2,matrix(1,nrow=2^(i-1),ncol=1))
	}

	dat <- rbind(c(min(y)-1,min(fx)-1,0,0),dat,c(max(y)+1,max(fx)+1,r+1,n+1))
	u <- c(min(y)-1,u,max(y)+1)
	uu <- 0:(r+1)
	part <- cbind(rep(1,2^r),part,rep(1,2^r))
	out <- c(0,out,0)

	foreachPartition <- function(s,vurocs,out,uu,dat,part,n,r) {

  	Z11 <- uu[as.logical(part[s,])]
  	p <- 1
  	V <- list(0)
  
  	for (k in 1:(r+2)) {
  	  if (sum(uu[k] == Z11) > 0) {
  	    V[[p]] <- dat[dat[,3]==uu[k], ,drop=FALSE]
  	    p <- p+1
  	  }
  	}
  	len <- rep(0,length(Z11)-1)
  
  	for (k in 1:(length(Z11)-1)) {
  	  len[k] <- nrow(V[[k]])*nrow(V[[k+1]])
  	}
  	len <- c(0,cumsum(len))
  	weight <- rep(0,len[length(Z11)])
  
  	for (p in 1:(length(Z11)-1)) {
  	  K <- Z11[p]
  	  L <- Z11[p+1]
  	  if (L==(K+1)) {
  	    for (a in 1:nrow(V[[p]])) {
  	      for (b in 1:nrow(V[[p+1]])) {
  	        if (V[[p]][a,2]<V[[p+1]][b,2]) {
  	          weight[b+(a-1)*nrow(V[[p+1]])+len[p]] <- 1
  	        }
  	      }
  	    }
  	  } else {
  	    int <- 0
  	    for (i in V[[p]][,4]) {
  	      w <- rep(0,L-K-1)
  	      for (j in (i+1):(n+1)) {
  	        m <- dat[j+1,3]
  	        if (m == (K+1)) {
  	          w[1] <- w[1]+1
  	        }
  	        if ((K+1)<m & m<L) {
  	          w[m-K] <- w[m-K] + w[m-K-1]
  	        }
  	        if (m == L) {
  	          weight[len[p] + int*nrow(V[[p+1]]) + out[j+1] +1] <- w[L-K-1]
  	        }
  	      }
  	      int <- int + 1
  	    }
  	  }
  	}
  	gamma <- rep(0,n+2)
  	gamma[1] <- 1
  
  	for (k in 1:(length(Z11)-1)) {
  	  out1 <- 0
  	  for (a in V[[k]][,4]) {
  	    out1 <- out1 + 1
  	    out2 <- 0
  	    for (b in V[[k+1]][,4]) {
  	      out2 <- out2 + 1
  	      gamma[b+1] <- gamma[b+1] + gamma[a+1]*weight[out2+(out1-1)*nrow(V[[k+1]])+len[k]]^2
  	    }
  	  }
  	}
  	GammaRes <- gamma[n+2]
  	Z2 <- uu[!as.logical(part[s,])]
  	n.under <- prod(vurocs[[2]][Z2])*prod(vurocs[[2]])
  	prod(vurocs[[2]][Z2]-1)*(GammaRes/n.under-vurocs[[1]]^2)
	}


	if (ncores==1) {
	  sigma2 <- sapply(1:2^r,foreachPartition,vurocs,out,uu,dat,part,n,r)
	} else {
  	cl <- makeCluster(min(ncores,detectCores()), type=clusterType )
  	registerDoParallel(cl)
  	sigma2<-foreach(i=1:2^r,.combine = c) %dopar% foreachPartition(i,vurocs,out,uu,dat,part,n,r)
  	on.exit(stopCluster(cl))
	}

	sigma2 <- sum(sigma2)/prod(vurocs[[2]])
	
	return(list(var=sigma2,val=vurocs[[1]]))
}