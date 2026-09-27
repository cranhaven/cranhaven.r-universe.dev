#' @title Bartlett's Test for One Sample Covariance Matrix
#'
#' @description
#' \code{Bcov} function tests whether the covariance matrix is equal to a 
#' given matrix or not.
#'
#' @importFrom stats cor cov pchisq pf pnorm qchisq qf var
#' @details
#' This function computes  Bartlett's test statistic for the covariance 
#' matrix of one sample.
#' 
#' @param data a data frame.
#' @param Sigma The covariance matrix in NULL hypothesis.
#'
#' @export
#' @return a list with 3 elements:
#' \item{ChiSquare}{The value of Test Statistic}
#' \item{df}{The Chi-Square statistic's degree of freedom}
#' \item{p.value}{p value}
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @references Rencher, A. C. (2003). Methods of multivariate analysis 
#' (Vol. 492). John Wiley & Sons.
#' @examples
#' 
#' data(iris) 
#' S<-matrix(c(5.71,-0.8,-0.6,-0.5,-0.8,4.09,-0.74,-0.54,-0.6,
#'      -0.74,7.38,-0.18,-0.5,-0.54,-0.18,8.33),ncol=4,nrow=4)
#' result <- Bcov(data=iris[,1:4],Sigma=S)
#' summary(result)

 Bcov<-function(data,Sigma){
 Name<-"Bcov"
 

 data<-as.matrix(data)
 N<-nrow(data)
 p<-ncol(data)
 dimensionSigma<-nrow(Sigma)
 
 if(p!=dimensionSigma)
        stop("The dimension of Sigma matrix must be equal to the number 
             of variables!")


 S<-cov(data)
 det.S<-det(S)
 det.S0<-det(Sigma)
 
 U<-(N-1)*(log(det.S0)-log(det.S)+sum(diag(S%*%solve(Sigma)))-p)
 df<-0.5*p*(p+1)
 
 if (N>=50){
 pval<-pchisq(U, df=df,lower.tail = FALSE)
 } else {
 U.adj<-U*(1-((1/(6*(N-1)-1))*(2*p+1-(2/(p+1))))) 
 pval<- pchisq(U.adj, df=df,lower.tail = FALSE)
 }

 results <- list(Chisq=U,df=df,p.value=pval,Test=Name)
 class(results)<-c("MVTests","list")
 return(results)
 }

