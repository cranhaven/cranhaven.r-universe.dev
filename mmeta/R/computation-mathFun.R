
is_integer <- function(x){
  return(x%%1==0)
}


supoort<-function(x,parm){
  return(all(x>0)&all(x<1))
}


logit <- function(p) log(p/(1-p))


expit <- function(x) exp(x)/(1+exp(x))


rhoBoundarySarmanov <- function(a1, b1, a2, b2){
  cc <- sqrt(a1*a2*b1*b2)/sqrt((a1+b1+1)*(a2+b2+1))
  upper_bound <- cc/max(a1*b2, a2*b1)
  lower_bound <- -cc/max(a1*a2, b1*b2)
  rho_range<-c(lower_bound,upper_bound)
  names(rho_range)<-c("lower_bound","upper_bound")
  return(rho_range)
}


liklihoodRatioTest <- function(loglik_H0, loglike_H1){

  chi2_value <- (-2*(loglik_H0 - loglike_H1))
  p_value <- pchisq(chi2_value,
                          df=1, lower.tail=F)
  return(list(chi2_value=chi2_value, p_value=p_value))
}

minlength_CI <- function(x, alpha, left=-1E10, right=1E10){
  n <- length(x)
  sortx <- sort(c(x, left, right))
  disx <- sortx[2:(n+2)]-sortx[1:(n+1)]
  xn <- ceiling((n+1)*alpha)
  xnsum <- cbind(1:xn, cumsum(disx[1:xn])+sum(disx[(n-xn+2):(n+1)])-cumsum(c(0, disx[(n-xn+2):n])))
  nleft <- mean(xnsum[xnsum[, 2]==max(xnsum[, 2]), 1])
  CItemp <- quantile(sortx, probs=c(alpha*(nleft-1)/(xn-1), 1-alpha+alpha*(nleft-1)/(xn-1)))
  CItemp[1] <- max(left,  CItemp[1])
  CItemp[2] <- min(right,  CItemp[2])
  return(CItemp)
}


###################################################################################################
### Purpose: This function calculates the inverse of a symmetric matrix (e.g. Hessian/Information)
###           It is standardized first to avoid computational problem
### Input: symmetric matrix: Hessian/Information : parts of the results of optim
### Output: inverse matrix or warming
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################
inverseMatrixFunc <- function(Mat){
  ## Mat: a square symmetric matrix with positive diagonal elements
  ## checking Mat to be correctly defined
  ## (i.e. no NA/Infinite, symmetric, no negative or zero diagonal element)
  ## if not, the inverse is a matrix of the same dimension of Mat with NAs

  checkHesesianMat(Mat)

  mat_inverse <- matrix(NA, nrow=nrow(Mat), ncol=ncol(Mat))


    mat_inverse <- matrix(NA, nrow=nrow(Mat), ncol=ncol(Mat))
    mat_stand <- diag(1/sqrt(diag(Mat)))%*%Mat%*%diag(1/sqrt(diag(Mat)))
    mat_rank <- qr(mat_stand)$rank
    if(mat_rank==nrow(Mat)){
      mat_inverse <- diag(1/sqrt(diag(Mat)))%*%solve(mat_stand)%*%diag(1/sqrt(diag(Mat)))
    } else{
      stop('The hessian matrix is not full rank.')
    }

  return(mat_inverse)
}


###################################################################################
### Purpose: Calulate the hypergeometric function 2f1 using Fortran library
### input: the parameters required by hypergeometric function 2f1
### Output: the value of hypergeoFun at X=x
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################
hypergeoFun <- function(aa, bb, cc, xx, YY=0) {
 #   dyn.load(SHARED_LIBRARY_PATH)
 #   return(.Fortran("hygfx", a=as.double(aa),b=as.double(bb),c=as.double(cc),
 #                  x=as.double(xx),y=as.double(YY),PACKAGE="mmeta")$y)

  .Fortran("hygfx", a=as.double(aa),b=as.double(bb),c=as.double(cc),
           x=as.double(xx),y=as.double(YY),PACKAGE="mmeta")$y
}

###################################################################################
### Purpose: compute the omegas used in the posterior distribution of p1 and p2
###          it is identical for OR/RR/RD.
### input: data (y1, n1, y2, n2), hyperparameters(a1,b1,a2,b2,rho)
### Output: the value of hypergeoFun at X=x
### Author:  Sheng Luo, Yong Chen, Xiao Su, Haitao Chu
### Data:    7/13/2012
###################################################################################
omegaCal <- function(y1, n1, y2, n2, a1, b1, a2, b2, rho) {
  alpha1 <- y1+a1
  beta1 <- n1-y1+b1
  alpha2 <- y2+a2
  beta2 <- n2-y2+b2

  # mu1, mu2: marginal means of p1 and p2
  mu1 <- a1/(a1+b1); mu1.1 <- 1-mu1
  mu2 <- a2/(a2+b2); mu2.1 <- 1-mu2

  # delta1, delta2: marginal sd of p1 and p2
  delta1 <- sqrt(mu1*mu1.1/(a1+b1+1))
  delta2 <- sqrt(mu2*mu2.1/(a2+b2+1))

  # myd: d=(mu1*mu2)/(delta1*delta2)
  myd <- (mu1*mu2)/(delta1*delta2)

  ## v1-v4 are weights
  v2 <- v3 <- -rho*myd
  v1 <- 1 - v2
  v4 <- -v2

  temp1 <- (lgamma(alpha1)+lgamma(beta1)+lgamma(alpha2)+lgamma(beta2)
            +lgamma(a1+b1)+lgamma(a2+b2)
            -(lgamma(a1)+lgamma(b1)+lgamma(a2)+lgamma(b2)
              +lgamma(alpha1+beta1)+lgamma(alpha2+beta2)))
  temp2 <- (lgamma(alpha1+1)+lgamma(beta1)+lgamma(alpha2)+lgamma(beta2)
            +lgamma(a1+b1+1)+lgamma(a2+b2)
            -(lgamma(a1+1)+lgamma(b1)+lgamma(a2)+lgamma(b2)
              +lgamma(alpha1+beta1+1)+lgamma(alpha2+beta2)))
  temp3 <- (lgamma(alpha1)+lgamma(beta1)+lgamma(alpha2+1)+lgamma(beta2)
            +lgamma(a1+b1)+lgamma(a2+b2+1)
            -(lgamma(a1)+lgamma(b1)+lgamma(a2+1)+lgamma(b2)
              +lgamma(alpha1+beta1)+lgamma(alpha2+beta2+1)))
  temp4 <- (lgamma(alpha1+1)+lgamma(beta1)+lgamma(alpha2+1)+lgamma(beta2)
            +lgamma(a1+b1+1)+lgamma(a2+b2+1)
            -(lgamma(a1+1)+lgamma(b1)+lgamma(a2+1)+lgamma(b2)
              +lgamma(alpha1+beta1+1)+lgamma(alpha2+beta2+1)))

  eps <- 1e-30
  if (abs(v1) < eps) {
    omega1 <- 0
  } else {
    omega1 <- 1/(1 + v2/v1*exp(temp2-temp1) + v3/v1*exp(temp3-temp1) + v4/v1*exp(temp4-temp1))
  }
  if (abs(v2) < eps) {
    omega2 <- 0
  } else {
    omega2 <- 1/(v1/v2*exp(temp1-temp2) + 1 + v3/v2*exp(temp3-temp2) + v4/v2*exp(temp4-temp2))
  }
  if (abs(v3) < eps) {
    omega3 <- 0
  } else {
    omega3 <- 1/(v1/v3*exp(temp1-temp3) + v2/v3*exp(temp2-temp3) + 1 + v4/v3*exp(temp4-temp3))
  }
  if (abs(v4) < eps) {
    omega4 <- 0
  } else {
    omega4 <- 1/(v1/v4*exp(temp1-temp4) + v2/v4*exp(temp2-temp4) + v3/v4*exp(temp3-temp4) + 1)
  }

  return(list(omega1=omega1, omega2=omega2, omega3=omega3, omega4=omega4))
}

