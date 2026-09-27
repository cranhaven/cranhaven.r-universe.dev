#' @title Bartlett's Sphericity Test
#'
#' @description
#' \code{Bsper} function tests whether a correlation matrix is equal to 
#' the identity matrix or not.
#'
#' @details
#' This function computes  Bartlett's test statistic for Sphericity Test. 
#' The hypotheses are \code{H0:R is equal to I} and \code{H1:R is not equal to I.}
#'
#' @param data a data frame.
#'
#' @importFrom stats cor cov pchisq pf pnorm qchisq qf var
#' @export
#' @return a list with 4 elements:
#' \item{ChiSquare}{The value of Test Statistic}
#' \item{df}{The Chi-Square statistic's degree of freedom}
#' \item{p.value}{p value}
#' \item{R}{Correlation matrix}
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @references Tatlidil, H. (1996). Uygulamali Cok Degiskenli Istatistiksel Yontemler. Cem Web.
#' @examples
#' data(iris) 
#' results <- Bsper(data=iris[,1:4])
#' summary(results)

 Bsper<-function(data){
 Name<-"Bsper"
 data<-as.matrix(data)
 n<-nrow(data)
 p<-ncol(data)
 R<-cor(data)
 Chisq<--((n-1)-(2*p+5)/6)*log(det(R)) 
 df<-(p*(p-1))/2
 pval<-pchisq(Chisq, df=df,lower.tail = FALSE)
 
 results <- list(Chisq=Chisq,df=df,p.value=pval,R=R,Test=Name)
 class(results)<-c("MVTests","list")
 return(results)
 }


