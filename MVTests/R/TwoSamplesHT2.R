#' @title Two Independent Samples Hotelling T^2 Test
#'
#' @description
#' \code{TwoSamplesHT2} function computes Hotelling T^2 statistic for two 
#' independent samples and gives confidence intervals.
#'
#' @details
#' This function computes two independent samples Hotelling T^2 statistics
#' that is used to test   
#' whether two population mean vectors are equal to each other.
#' When \code{H0} is rejected, this function computes confidence intervals
#' for all variables to determine variable(s) affecting on rejection decision.
#' Moreover, when covariance matrices are not homogeneity, the approach proposed
#' by D. G. Nel and V. D. Merwe (1986) is used.
#' 
#' @importFrom stats cor cov pchisq pf pnorm qchisq qf var
#' @param data a data frame.
#' @param group a group vector consisting of 1 and 2 values.
#' @param alpha Significance Level that will be used for confidence intervals. 
#' default=0.05
#' @param Homogenity a logical argument. If sample covariance matrices are 
#' homogeneity,then \code{Homogenity=TRUE}. Otherwise \code{Homogenity=FALSE} 
#' The homogeneity of covariance matrices can be investigated with \code{BoxM} function.
#' @export
#'
#' @return a list with 8 elements:
#' \item{HT2}{The value of Hotelling T^2 Test Statistic}
#' \item{F}{The value of F Statistic}
#' \item{df}{The F statistic's degree of freedom}
#' \item{p.value}{p value}
#' \item{CI}{The lower and upper limits of confidence intervals obtained for 
#' all variables}
#' \item{alpha}{The alpha value using in confidence intervals}
#' \item{Descriptive1}{Descriptive Statistics for the first group}
#' \item{Descriptive2}{Descriptive Statistics for the second group}
#' @references Rencher, A. C. (2003). Methods of multivariate analysis 
#' (Vol. 492). John Wiley & Sons.
#' @references Tatlidil, H. (1996). Uygulamali Cok Degiskenli Istatistiksel 
#' Yontemler. Cem Web.
#' @references D.G. Nel & C.A. Van Der Merwe (1986) A solution to the 
#' multivariate behrens fisher problem, Communications in Statistics:Theory and
#'  Methods, 15:12, 3719-3735
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#' 
#' data(iris)
#' G<-c(rep(1,50),rep(2,50))
#' # When covariances matrices are homogeneity
#' results1 <- TwoSamplesHT2(data=iris[1:100,1:4],group=G,alpha=0.05)
#' summary(results1)
#' # When covariances matrices are not homogeneity
#' results2 <- TwoSamplesHT2(data=iris[1:100,1:4],group=G,Homogenity=FALSE)
#' summary(results2)


 TwoSamplesHT2<-function(data,group,alpha=0.05,Homogenity=TRUE){
 Name<-"TwoSamplesHT2"
 
 # When Homogenity=TRUE
 if (Homogenity=="TRUE") {
 data<-as.matrix(data)
 X1<-data[which(group==1),]
 X2<-data[which(group==2),]
 n1<-nrow(X1); n2<-nrow(X2);n<-n1+n2
 p<-ncol(data)
 X1.mean<-colMeans(X1);  X2.mean<-colMeans(X2) 
 diff<-X1.mean-X2.mean
 S1<-cov(X1);  S2<-cov(X2) 
 Sigma<-(1/(n1+n2-2))*((n1-1)*S1+(n2-1)*S2)
 T2<-(n1*n2/n)*(t(diff)%*%solve(Sigma)%*%(diff))
 F<-((n-p-1)/(p*(n-2)))*T2
 T2.table<-((p*(n-2))/(n-p-1))*qf(df1=p,df2=n-p-1,p=alpha,lower.tail=FALSE)
 pval<-pf(F, df1=p, df2=(n-p-1), lower.tail = FALSE)
 df<-c(p,n-p-1)
 
 Low<-Up<-imp<-NULL
 for (i in 1:p) {
 a<-c(rep(0,p));a[i]<-1
 Low[i]<-a%*%diff-sqrt(T2.table*(n/(n1*n2))*Sigma[i,i])
 Up[i]<-a%*%diff+sqrt(T2.table*(n/(n1*n2))*Sigma[i,i])
 if (Low[i]<=0& Up[i]>=0) {
 imp[i]<-"FALSE"
 }else {
 imp[i]<-"*TRUE*"
 } }
 CI<-data.frame(Lower=Low,Upper=Up,Import=imp)
 colnames(CI)<-c("Lower","Upper","Important Variables?")
 rownames(CI)<-colnames(data)
 
 } else {  # When Homogenity=FALSE
 data<-as.matrix(data)
 X1<-data[which(group==1),]
 X2<-data[which(group==2),]
 n1<-nrow(X1); n2<-nrow(X2);n<-n1+n2
 p<-ncol(data)
 X1.mean<-colMeans(X1);  X2.mean<-colMeans(X2) 
 diff<-X1.mean-X2.mean
 S1<-cov(X1);  S2<-cov(X2) 
 SS1<-S1*(1/n1); SS2<-S2*(1/n2)
 S<-SS1+SS2
 T2<-(t(diff)%*%solve(S)%*%(diff))

 B1<-(sum(diag((SS1%*%SS1)))+(sum(diag((SS1))))^2)/(n1-1)
 B2<-(sum(diag((SS2%*%SS2)))+(sum(diag((SS2))))^2)/(n2-1)
 
 pay<-sum(diag((S%*%S)))+(sum(diag((S))))^2 
 payda<-B1+B2
 
 v<-pay/payda    #Equation 4.7 (Nel and Merwe, 1986)

 F<-((v-p+1)/(p*v))*T2
 T2.table<-((p*v)/(v-p+1))*qf(df1=p,df2=v-p+1,p=alpha,lower.tail=FALSE)
 pval<-pf(F, df1=p, df2=(v-p+1), lower.tail = FALSE)
 df<-c(p,v-p+1)
 
 Low<-Up<-imp<-NULL
 for (i in 1:p) {
 a<-c(rep(0,p));a[i]<-1
 Low[i]<-a%*%diff-sqrt(T2.table*S[i,i])
 Up[i]<-a%*%diff+sqrt(T2.table*S[i,i])
 if (Low[i]<=0& Up[i]>=0) {
 imp[i]<-"FALSE"
 }else {
 imp[i]<-"*TRUE*"
 } }
 CI<-data.frame(Lower=Low,Upper=Up,Import=imp)
 colnames(CI)<-c("Lower","Upper","Important Variables?")
 rownames(CI)<-colnames(data)
  

 }

 #1. Group
 Desc1<-rbind(X1.mean,sqrt(diag(S1)))
 colnames(Desc1)<-colnames(X1)
 rownames(Desc1)<-c("Means","Sd")

 #2. Group
 Desc2<-rbind(X2.mean,sqrt(diag(S2)))
 colnames(Desc2)<-colnames(X2)
 rownames(Desc2)<-c("Means","Sd")

 results <- list(HT2=T2,F=F, df=df,p.value=pval,CI=CI,alpha=alpha,
                 Descriptive1=Desc1,Descriptive2=Desc2,Test=Name)
 class(results)<-c("MVTests","list")
 return(results)
 }

