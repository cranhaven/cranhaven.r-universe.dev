#' @title Box's M Test
#'
#' @description
#' \code{BoxM} function tests whether the covariance matrices of independent 
#' samples are equal or not.
#'
#' @details
#' This function computes  Box-M test statistic for the covariance matrices of independent samples.  
#' The hypotheses are defined as H0:The Covariance matrices are homogeneous and 
#' H1:The Covariance matrices are not homogeneous 
#' 
#' @importFrom stats cor cov pchisq pf pnorm qchisq qf var
#' @param data a data frame.
#' @param group grouping vector.
#'
#' @export
#' @return a list with 3 elements:
#' \item{ChiSquare}{The value of Test Statistic}
#' \item{df}{The Chi-Square statistic's degree of freedom}
#' \item{p.value}{p value}
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @references Rencher, A. C. (2003). Methods of multivariate analysis (Vol. 492). John Wiley & Sons.
#' @examples
#' 
#' data(iris) 
#' results <- BoxM(data=iris[,1:4],group=iris[,5])
#' summary(results)

 BoxM<-function(data,group){
 Name<-"BoxM"
 data<-as.matrix(data)
 group<-as.factor(group)
 g<-length(levels(group))
 Levels<-levels(group)
 N<-nrow(data)
 p<-ncol(data)
 
 ## Calculation S pooled ## 
 E<-matrix(0,p,p)
 ns<-NULL
 
 #Calculation E matrix
 for (i in 1:g) {
 ns[i]<-length(which(group==Levels[i]))
 Si<-cov(data[which(group==Levels[i]),])
 E<-E+(ns[i]-1)*Si
 }
 
 # Calculation Spl
 S.pooled<-(1/(N-g))*E

 ## Calculation M using Equation (7.21) ##  
 M=1
 for (i in 1:g){
 Si<-cov(data[which(group==Levels[i]),]) 
 M<-M*(det(Si)/det(S.pooled))^((ns[i]-1)/2)
 }
 
 #Calculation C
 k1<-(2*p^2+3*p-1)/(6*(p+1)*(g-1))  #ns are different 
 k2<-((g+1)*(2*p^2+3*p-1))/(6*g*(p+1)*(N/g-1))  #ns are same-Equation 7.25
 T=0
 if (all(ns==mean(ns))==TRUE) { #ns are same
 C<-k2
 } else {
 for (i in 1:g)	{
 T<-T+(1/(ns[i]-1))
 }
 C<-k1*(T-(1/(N-g)))  # Equation 7.22
 }

 # Calculation U Statistic
 U<--2*(1-C)*log(M)   #Equation 7.23
 
 df<-(0.5*p*(p+1)*(g-1))
 pval<-pchisq(U, df=df,lower.tail = FALSE)
 
 results <- list(Chisq=U,df=df,p.value=pval,Test=Name)
 class(results)<-c("MVTests","list")
 return(results)
 }

