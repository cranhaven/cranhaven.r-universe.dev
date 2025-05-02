absodevimean <- function(x){
  mu <- mean(x)
  median(abs(x-mu))
}

absodevimedian <- function(x){
  mu <- median(x)
  median(abs(x-mu))
}



#estimated the kurtosis
estkurtosis <- function(x){
  p <- dim(x)[2]
  kappa <- rep(0,p)
  for (j in 1:p){
    new_j <- x[,j]-mean(x[,j])
    upper <- mean(new_j^4)
    bottom <- mean(new_j^2)
    kappa[j] <- upper/(bottom^2)/3-1
  }
  return(mean(kappa))
}



Threshold <- function(x,y,S,C,n, cutoff)
{
  r <- pcorOrder(x,y, S, C)
  temp_cutoff <- exp(2*(cutoff)/sqrt(n-3-length(S)))
  T <- (temp_cutoff-1)/(temp_cutoff+1)
  is.na(T) || abs(r) <= T
}


pcorOrder <- function(i,j, k, C, cut.at = 0.9999999) {
  ## Purpose: Compute partial correlation
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## - i,j,k: Partial correlation of i and j given k
  ## - C: Correlation matrix among nodes
  ## ----------------------------------------------------------------------
  ## Original idea: Markus Kalisch

  if (length(k) == 0) {
    r <- C[i,j]
  } else if (length(k) == 1) {
    r <- (C[i, j] - C[i, k] * C[j, k])/sqrt((1 - C[j, k]^2) * (1 - C[i, k]^2))
  } else { ## length(k) >= 2
    PM <- corpcor::pseudoinverse(C[c(i,j,k), c(i,j,k)])
    r <- -PM[1, 2]/sqrt(PM[1, 1] * PM[2, 2])
  }
  if(is.na(r)) 0 else min(cut.at, max(-cut.at, r))
}




pcorOrder_recusive <- function(i,j, k, C, cut.at = 0.9999999) {
  ## Purpose: Compute partial correlation
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## - i,j,k: Partial correlation of i and j given k
  ## - C: Correlation matrix among nodes
  ## ----------------------------------------------------------------------
  ## Author: Markus Kalisch, Date: 26 Jan 2006; Martin Maechler
  if (length(k) == 0) {
    r <- C[i,j]
  } else if (length(k) == 1) {
    r <- (C[i, j] - C[i, k] * C[j, k])/sqrt((1 - C[j, k]^2) * (1 - C[i, k]^2))
  } else { ## length(k) >= 2
    m = k[1]
    k_m = k[2:length(k)]
    pcor_ijkm <- pcorOrder_recusive(i,j,k_m,C)
    pcor_imkm <- pcorOrder_recusive(i,m,k_m,C)
    pcor_jmkm <- pcorOrder_recusive(j,m,k_m,C)
    r <- (pcor_ijkm - pcor_imkm * pcor_jmkm)/sqrt((1 - pcor_imkm^2) * (1 - pcor_jmkm^2))
  }
  if(is.na(r)) 0 else min(cut.at, max(-cut.at, r))
}

#smooth y wiht local linear approach
yprediction <- function(tgrid, degree, y, ttimes, kernel){
  len <- length(tgrid)
  tempy <- rep(0,len)
  h <- KernSmooth::dpill(ttimes,y)

  for (i in 1:len){
    temp <- polynomial(tgrid[i], degree, y, ttimes, kernel,h)[[1]]
    tempy[i] <- temp[1,1]
  }
  return(tempy)
}

polynomial <- function(tpoint, degree, y, ttimes, kernel,h){

  # construct the X matrix
  xmatrix <- matrix(1,ncol=degree+1,nrow=length(ttimes))
  for (i in 1:degree){
    xmatrix[,(i+1)] <- (ttimes-tpoint)^i
  }

  # construct the weight matrix
  wmatrix <- diag(kernel((ttimes-tpoint)/h)/h)

  betas <- solve(t(xmatrix)%*% wmatrix %*% xmatrix)%*%t(xmatrix)%*%wmatrix%*%y

  return(list(betas,xmatrix))
}



#####################################################################
# get samples for partially linear models
#####################################################################

#' A function to generate toy partial linear model data
#'
#' @export
generate_toy_pldata <- function(){
  p = 30
  n = 200
  truebeta <- c(c(3,1.5,0,0,2),rep(0,p-5))
  sigma <- matrix(0.5,p+1,p+1)
  diag(sigma) <- 1
  sigma_x <- sigma[1:p,1:p]
  myeig <- eigen(sigma)
  sigmahalf <- myeig$vectors %*% sqrt(diag(myeig$values))%*% t(myeig$vectors)
  timesample_normal(p,sigmahalf, n, 0.25, truebeta)
}




timesample_normal <- function(p, sigmahalf, Nsamp, omega, beta){

  alphat <- function(t) t^2

  xsample <- matrix(0,nrow=Nsamp,ncol=p+2)
  timepoints <- rep(0,Nsamp)
  ysample <- rep(0,Nsamp)

  sigmahalf <- cbind(sigmahalf,rep(0,p+1))
  sigmahalf <- rbind(sigmahalf,c(rep(0,p+1),sqrt(omega)))

  for (j in 1:Nsamp){
    temp <- rnorm(p+2,0,1)
    xsample[j,] <- sigmahalf %*% temp
    timepoints[j] <- pnorm(xsample[j,1],0,1)
    ysample[j] <- alphat(timepoints[j]) + xsample[j,2:(p+1)] %*% as.matrix(beta) + xsample[j,(p+2)]
  }

  # ordered the records
  x_cov <- xsample[,2:(p+1)]
  orderrec <- order(timepoints)
  times <- timepoints[orderrec]
  ysample <- ysample[orderrec]
  return(list(ysample,x_cov[orderrec,],times))
}
