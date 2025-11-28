#' Generate simulated data for MTAFT analysis.
#'
#' This function generates simulated data for the MTAFT (Multi-Threshold Accelerated Failure Time) analysis based on a simple simulation procedure described in the article.
#'
#' @param n The number of sample size.
#' @param err The error distribution type, either "normal" or "t3".
#'
#' @return A dataset containing the simulated data for MTAFT analysis.
#' @export
#'
#' @examples
#' # Generate simulated data with 500 samples and normal error distribution
#' dataset <- MTAFT_simdata(n = 500, err = "normal")
#' Y <- dataset[, 1]
#' delta <- dataset[, 2]
#' Tq <- dataset[, 3]
#' X <- dataset[, -c(1:3)]
#'
#' # Generate simulated data with 200 samples and t3 error distribution
#' dataset <- MTAFT_simdata(n = 200, err = "t3")
#' Y <- dataset[, 1]
#' delta <- dataset[, 2]
#' Tq <- dataset[, 3]
#' X <- dataset[, -c(1:3)]
MTAFT_simdata <- function(n,err=c("normal","t3"))
{
  p = 3
  X = matrix(rnorm(n*p,0,1),n,p)
  C = rnorm(n,2,4)
  if(err == "normal"){
    e <- rnorm(n,0,sqrt(0.5))
  }else if(err == "t3"){
    e <- rt(n,3)/sqrt(6)
  }else{
    stop("Specify the correct error distribution.")
  }

  #Real threshods (qnorm(0.3),qnorm(0.6))=(-0.5244,0.2533)
  id1=which(X[,1]<=qnorm(0.3))
  id2=which(X[,1]<=qnorm(0.6) & X[,1]>qnorm(0.3))
  id3=which(X[,1]>qnorm(0.6))

  beta01=2
  beta11=c(rep(1,3))
  beta02=1
  beta12=c(-1,-1,0.5)
  beta03=0
  beta13=c(1,0.5,-1)
  #Real coefficeints:(beta01,beta11,beta02-beta01,beta12-beta11,beta03-beta02,beta13-beta12)
  #=(2,1,1,1,1,1,-1,0,0,-1,-1,-1,0,-1,1,0,0,0)
  X1=X[id1,]
  n1=length(id1)
  T1=X1%*%beta11+beta01 + e[1:n1] #+rnorm(n1,0,sqrt(0.5)) #
  C1=C[id1]
  delta1=C1
  Y1=T1
  for(i in 1:n1)
  {
    if(T1[i]<C1[i])
    {
      delta1[i]=1
    }
    if(T1[i]>=C1[i])
    {
      delta1[i]=0
      Y1[i]=C1[i]
    }
  }
  Z11=cbind(Y1,X1,delta1,C1)
  X2=X[id2,]
  n2=length(id2)
  T2=X2%*%beta12+beta02 + e[(n1+1):(n1+n2)] #+rnorm(n2,0,sqrt(0.5)) #
  C2=C[id2]
  delta2=C2
  Y2=T2
  for(i in 1:n2)
  {
    if(T2[i]<C2[i])
    {
      delta2[i]=1
    }
    if(T2[i]>=C2[i])
    {
      delta2[i]=0
      Y2[i]=C2[i]
    }
  }
  Z12=cbind(Y2,X2,delta2,C2)

  X3=X[id3,]
  n3=length(id3)
  T3=X3%*%beta13+beta03 + e[(n1+n2+1):(n1+n2+n3)]
  C3=C[id3]
  delta3=C3
  Y3=T3
  for(i in 1:n3)
  {
    if(T3[i]<C3[i])
    {
      delta3[i]=1
    }
    if(T3[i]>=C3[i])
    {
      delta3[i]=0
      Y3[i]=C3[i]
    }
  }
  Z13=cbind(Y3,X3,delta3,C3)
  ZZ=rbind(Z11,Z12,Z13)
  ## ZZ[,2] is the thresholding variable
  ord=order(ZZ[,2])
  ZZ=ZZ[ord,]
  n=dim(ZZ)[1]
  p=dim(ZZ)[2]-3
  Y=ZZ[,1]
  X=ZZ[,2:(p+1)]
  delta=ZZ[,p+2]
  Tq = sort(X[,1])
  dataset = cbind(Y,delta,Tq,X)

  return(dataset)

}

