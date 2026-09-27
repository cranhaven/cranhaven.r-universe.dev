#' @title Multivariate Paired Test
#'
#' @description
#' \code{Mpaired} function computes the value of test statistic based on 
#' Hotelling T Square 
#' approach in multivariate paired data sets.
#'
#' @details
#' This function computes one sample Hotelling T^2 statistics for paired 
#' data sets.
#' 
#' @importFrom stats cor cov pchisq pf pnorm qchisq qf var
#' @param T1 The first treatment data.
#' @param T2 The second treatment data.
#' @export
#' @return a list with 7 elements:
#' \item{HT2}{The value of Hotelling T^2 Test Statistic}
#' \item{F}{The value of F Statistic}
#' \item{df}{The F statistic's degree of freedom}
#' \item{p.value}{p value}
#' \item{Descriptive1}{The descriptive statistics of the first treatment}
#' \item{Descriptive2}{The descriptive statistics of the second treatment}
#' \item{Descriptive.Difference}{The descriptive statistics of the differences}
#' @references Rencher, A. C. (2003). Methods of multivariate analysis 
#' (Vol. 492). John Wiley & Sons.
#'
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#' 
#' data(Coated)
#' X<-Coated[,2:3]; Y<-Coated[,4:5]
#' result <- Mpaired(T1=X,T2=Y)
#' summary(result)


 Mpaired<-function(T1,T2){
 Name<-"Mpaired"
 n1<-nrow(T1); n2<-nrow(T2)
 if (n1!=n2)
      stop("The number of observations in the two groups must be equal to 
            each other!")
 
 p1<-ncol(T1); p2<-ncol(T2)

 if (p1!=p2)
      stop("The number of variables in the two groups must be equal to 
           each other!") 

  n<-n1; p=p1
  d<-T2-T1 
  d.mean<-colMeans(d)
  Sd<-cov(d)
  TSquare<-n*t(d.mean)%*%solve(Sd)%*%d.mean
  F<-(n-p)/(p*(n-1))*TSquare
  pval<-pf(F, df1=p, df2=n-p, lower.tail = FALSE)
  df<-c(p,n-p)
  
 #1. Treatment
 T1.mean<-colMeans(T1) ;S1<-cov(T1)
 Desc1<-rbind(T1.mean,sqrt(diag(S1)))
 colnames(Desc1)<-colnames(T1)
 rownames(Desc1)<-c("Means","Sd")

 #2. Treatment 
 T2.mean<-colMeans(T2) ;S2<-cov(T2) 
 Desc2<-rbind(T2.mean,sqrt(diag(S2)))
 colnames(Desc2)<-colnames(T2)
 rownames(Desc2)<-c("Means","Sd")

 #Difference 
 Desc.d<-rbind(d.mean,sqrt(diag(Sd)))
 colnames(Desc.d)<-colnames(d)
 rownames(Desc.d)<-c("Means","Sd")
 
 results <- list(HT2=TSquare,F=F, df=df,p.value=pval,Descriptive1=Desc1,
              Descriptive2=Desc2,Descriptive.Difference=Desc.d,Test=Name)
 class(results)<-c("MVTests","list")
 return(results)
 }
