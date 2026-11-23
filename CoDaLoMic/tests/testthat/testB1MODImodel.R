#context("B1MODImodel")


 df<-data.frame(cbind(c(0.1,0.1,0.8),c(0.2,0.1,0.7)))
 E=3
 EspecieMaxima=3
 set.seed(724)
 A=matrix(c(-2:3),2,3)
 Tt=2


 Coef1=rbind(c(1,1),
       c(log(0.1/0.8),log(0.1/0.8)),
       c(log(0.1)-log(0.8),log(0.1)-log(0.8)))

 Coef2=rbind(c(1,1),
             c(log(0.2/0.7),log(0.1/0.7)),
               c(log(0.1)-log(0.7),log(0.2)-log(0.7)))

 mu=matrix(0,2,2)
 mu[1,1]=A[1,]%*%Coef1[,1]
 mu[2,1]=A[2,]%*%Coef1[,2]
 mu[1,2]=A[1,]%*%Coef2[,1]
 mu[2,2]=A[2,]%*%Coef2[,2]

 test_that("B1MODImodel", {
   expect_equal(B1MODImodel(A,df, E, EspecieMaxima,Tt),mu, tolerance=1e-6)
 })
