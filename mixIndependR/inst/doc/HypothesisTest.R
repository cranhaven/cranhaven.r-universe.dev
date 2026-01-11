## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load package,include = FALSE---------------------------------------------
library(mixIndependR)
library(ggplot2)

## ----preparation, include=FALSE-----------------------------------------------
x <- mixexample

p <- AlleleFreq(x)
h <-Heterozygous(x)
H <- RxpHetero(h,p,HWE=FALSE)
AS<-AlleleShare(x,replacement=FALSE)
e <-RealProAlleleShare(AS)
ObsDist_K<-FreqHetero(h)
ExpDist_K<- DistHetero(H)
ObsDist_X<-FreqAlleleShare(AS)
ExpDist_X<-DistAlleleShare(e)

## ----Simulation---------------------------------------------------------------
Simu_K <- Simulate_DistK(H,100,500)
Simu_X <- Simulate_DistX(e,100,500)

## ----Chi-square---------------------------------------------------------------
x2_K<-Dist_SimuChisq(Simu_K,ExpDist_K$Density,200)
x2_X<-Dist_SimuChisq(Simu_X,ExpDist_X$Density,200)
P1<-ecdf(x2_K)
P2<-ecdf(x2_X)

## ----Last plot, echo=FALSE, fig.height=4, fig.show='hold', fig.width=3--------
x <- c(0:200)
dfX2 <- data.frame(x=x,y=P1(x))
ggplot(dfX2,aes(x=x,y=P1(x)))+
  geom_line()+
  geom_hline(yintercept = 0.95,color="Red")+
  ggtitle("CPF No. of Heterozygous Loci")+
  xlab("Chi-square")+ylab("1-p-value")

dfX22 <- data.frame(x=x,y=P2(x))
ggplot(dfX22,aes(x=x,y=P2(x)))+
  geom_line()+
  geom_hline(yintercept = 0.95,color="Red")+
  ggtitle("CPF No. of Shared Alleles")+
  xlab("Chi-square")+ylab("1-p-value")

