#' @name FanPC
#' @title Apply the FanPC method to the Laplace factor model
#' @description This function performs Factor Analysis via Principal Component (FanPC) on a given data set. It calculates the estimated factor loading matrix (AF), specific variance matrix (DF), and the mean squared errors.
#' @param data A matrix of input data.
#' @param m is the number of principal component
#' @return AF,DF,SigmahatF
#' @examples
#' library(LaplacesDemon)
#' library(MASS)
#' n=1000
#' p=10
#' m=5
#' mu=t(matrix(rep(runif(p,0,1000),n),p,n))
#' mu0=as.matrix(runif(m,0))
#' sigma0=diag(runif(m,1))
#' F=matrix(mvrnorm(n,mu0,sigma0),nrow=n)
#' A=matrix(runif(p*m,-1,1),nrow=p)
#' lanor <- rlaplace(n*p,0,1)
#' epsilon=matrix(lanor,nrow=n)
#' D=diag(t(epsilon)%*%epsilon)
#' data=mu+F%*%t(A)+epsilon
#' results <- FanPC(data, m)
#' print(results)
#' @export
#' @importFrom matrixcalc frobenius.norm
#' @importFrom stats cor cov
FanPC=function(data,m){
  X=scale(data)
  n=nrow(X)
  SigmahatF=cor(X)
  eig<-eigen(SigmahatF)
  lambdahat = eig$values[1:m]
  ind<-order(lambdahat,decreasing=T)
  lambdahat<-lambdahat[ind]
  Q<- eig$vectors
  Q<-Q[,ind]
  AF<-Q[,1:m]
  hF <- diag(AF %*% t(AF))
  DF <- diag(SigmahatF - hF)
  return(
    list(AF=AF,DF=DF,SigmahatF=SigmahatF))}

