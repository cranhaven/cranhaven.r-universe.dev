#' @title One Sample Hotelling T^2 Test
#'
#' @description
#' \code{OneSampleHT2} computes one sample Hotelling T^2 statistics and gives
#' confidence intervals
#'
#' @details
#' This function computes one sample Hotelling T^2 statistics that is used to 
#' test whether population mean vector is equal to a vector given by a user.
#' When \code{H0} is rejected, this function computes confidence intervals
#' for all variables.
#' 
#' @importFrom stats cor cov pchisq pf pnorm qchisq qf var
#' @param data a data frame.
#' @param mu0 mean vector that is used to test whether population mean 
#' parameter is equal to it.
#' @param alpha Significance Level that will be used for confidence intervals.
#' \code{default alpha=0.05}.
#' @export
#' @return a list with 7 elements:
#' \item{HT2}{The value of Hotelling T^2 Test Statistic}
#' \item{F}{The value of F Statistic}
#' \item{df}{The F statistic's degree of freedom}
#' \item{p.value}{p value}
#' \item{CI}{The lower and upper limits of confidence intervals obtained 
#' for all variables}
#' \item{alpha}{The alpha value using in confidence intervals}
#' \item{Descriptive}{Descriptive Statistics}
#' @references Rencher, A. C. (2003). Methods of multivariate analysis 
#' (Vol. 492). John Wiley & Sons.
#' @references Tatlidil, H. (1996). Uygulamali Cok Degiskenli Istatistiksel 
#' Yontemler. Cem Web.
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#' data(iris)
#' 
#' mean0<-c(6,3,1,0.25)
#' result <- OneSampleHT2(data=iris[1:50,-5],mu0=mean0,alpha=0.05)
#' summary(result)

 OneSampleHT2<-function(data,mu0,alpha=0.05){
 Name<-"OneSampleHT2"
 data<-as.matrix(data)
 n<-nrow(data)
 p<-ncol(data)
 x.mean<-colMeans(data) 
 Sigma<-cov(data)
 T2<-n*(t(x.mean-mu0)%*%solve(Sigma)%*%(x.mean-mu0))
 F<-(n-p)/(p*(n-1))*T2
 T2.table<-((p*(n-1))/(n-p))*qf(df1=p,df2=n-p,p=alpha,lower.tail=FALSE)
 pval<-pf(F, df1=p, df2=n-p, lower.tail = FALSE)
 df<-c(p,n-p)
 Low<-Up<-imp<-NULL
 for (i in 1:p) {
 a<-c(rep(0,p));a[i]<-1
 Low[i]<-a%*%x.mean-sqrt(T2.table*Sigma[i,i]/n)
 Up[i]<-a%*%x.mean+sqrt(T2.table*Sigma[i,i]/n)

 if (mu0[i]>=Low[i]& mu0[i]<=Up[i]) {
 imp[i]<-"FALSE"
 }else {
 imp[i]<-"*TRUE*"
 } }
 CI<-data.frame(Lower=Low,Upper=Up,Mu0=mu0,Import=imp)
 colnames(CI)<-c("Lower","Upper","Mu0","Important Variables?") 
 rownames(CI)<-colnames(data)
 
 Desc<-rbind(rep(n,p),x.mean,sqrt(diag(Sigma)))
 colnames(Desc)<-colnames(data)
 rownames(Desc)<-c("N","Means","Sd")
 
 results <- list(HT2=T2,F=F, df=df,p.value=pval,CI=CI,alpha=alpha,
                Descriptive=Desc,Test=Name)
 class(results)<-c("MVTests","list")
 return(results)
 }
