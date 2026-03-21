
#'simulating dataset (Online settings)
#'
#'Function to simulate datasets based on the VAR(1) model (without intercept). The dataset include in control batches and new batches for online monitoring
#'
#'
#'
#' @param n  number of time-instants
#' @param I  number of in control batch samples
#' @param size  number of variables
#' @param Inew  number of new batch samples for monitoring
#' @param n1  number of time-instants for the new batches under the in control process
#' @param B1  matrix (size x size) containing the VAR(1) coefficients of in control batches
#' @param varcov  covariance matrix (size x size) of errors
#' @param B1new  matrix (size x size) containing the VAR(1) coefficients of new batches
#' @return \code{data}  A dataframe of reference dataset of \code{I} in control batches.
#' For each batch, variables are arranged in lines and columns are time-instants. The different batches are combined in a single dataset
#' of dimension \code{[(I * size) X n]}
#' @return \code{newdata}  A dataframe including a dataset of \code{Inew} new batches for monitoring (each with same number of variables and time-instants of \code{data}).
#' The different batches are combined in a single dataset of dimension \code{[(Inew * size) X n]}
#' @examples
#'
#' # Example 1: Two variables (A default in control simulating dataset)
#'
#'     mydata=simon()
#'
#'
#' # Example 2: Three variables and Inew=5 out of control batches
#'
#'     B1=matrix(c(-0.3,0,0.4,0,0.2,0,0,-0.1,0.5),3,byrow=TRUE)
#'     B1new=matrix(c(0.1,0,0.4,0,0.2,0,0,-0.1,0.5),3,byrow=TRUE)
#'     mydata1=simon(n=100,I=200,size=3,Inew=5,n1=50,B1,varcov=diag(3),B1new)
#'
#'
#' # Example 3: Three variables and one Inew=1 out of control batch
#'
#'     B1=matrix(c(-0.3,0,0.4,0,0.2,0,0,-0.1,0.5),3,byrow=TRUE)
#'     B1new=matrix(c(0.1,0,0.4,0,0.7,0,0,-0.1,0.5),3,byrow=TRUE)
#'     mydata2=simon(n=100,I=1,size=3,Inew=1,n1=25,B1,varcov=diag(3),B1new)
#'     plot.ts(t(mydata2$data),main="One in control batch sample")
#'     plot.ts(t(mydata2$newdata),main="One new batch sample")
#'
#' @seealso onlinem, simoff, offlinem
#'
#' @export
simon=function(n=100,I=200,size=2,Inew=5,n1=50,B1=matrix(c(-0.3, 0.4, 0.4, 0.5), ncol=size, byrow=TRUE),varcov=diag(2),B1new=matrix(c(-0.3, 0.4, 0.4, 0.5), ncol=size ,byrow=TRUE)){

  if(size<2 || size >5){stop("Error ... number of variables (size) must be between 2 and 5")}
  if(dim(B1)[1]!=size){stop("Error ... number of variables (size) must be according to the dimension of B1 matrix")}
  if(dim(B1)[1]!=dim(B1)[2]){stop("Error ... B1 must be a square matrix")}
  if(dim(B1new)[1]!=dim(B1)[1]){stop("Error ... B1new must the same size of B1")}
  if(dim(B1new)[2]!=dim(B1)[2]){stop("Error ... B1new must the same size of B1")}
  if(dim(varcov)[1]!=dim(B1)[1]){stop("Error ... varcov must the same size of B1")}
  if(dim(varcov)[2]!=dim(B1)[2]){stop("Error ... varcov must the same size of B1")}
  if(n1>=(n-1)){stop("Error ... n1 must be less than n-1")}
  if(sum(varcov==t(varcov))!=dim(varcov)[2]^2){stop("Error ... varcov must be a symmetric matrix")}
  if(sum(abs(eigen(B1)$value)<1)<dim(B1)[1]){stop("Error ...The engenvalues of B1 must be smaller than 1")}
  if(sum(abs(eigen(B1new)$value)<1)<dim(B1new)[1]){stop("Error ...The engenvalues of B1new must be smaller than 1")}

  #data

  size=size
  data=matrix(0,ncol=n, nrow=1)
  for(i in 1:I){
    v1<-VAR.sim(B=B1,n=n, lag=1, include="none",varcov=varcov)
    data=rbind(data,t(v1))
  }
  data=data[-1,]

  #new data
  newdata=matrix(0,ncol=n, nrow=1)
  for(i in 1:Inew){
   v1.1<-VAR.sim(B=B1,n=n1, lag=1, include="none",varcov=varcov)
   v1.2<-VAR.sim(B=B1new,n=(n-n1), lag=1, include="none",varcov=varcov)
   v1=rbind(v1.1,v1.2)
   newdata=rbind(newdata,t(v1))
  }
  newdata=newdata[-1,]


  return(list(data=data,newdata=newdata))
}

