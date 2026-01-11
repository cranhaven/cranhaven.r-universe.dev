## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load, include = FALSE----------------------------------------------------
library(mixIndependR)
library(ggplot2)

## ----include=FALSE------------------------------------------------------------
x <- mixexample
p <- AlleleFreq(x)
h <-Heterozygous(x)
H <- RxpHetero(h,p,HWE=FALSE)
AS<-AlleleShare(x,replacement =FALSE)
e <-RealProAlleleShare(AS)

## -----------------------------------------------------------------------------
ObsDist_K<-FreqHetero(h)
ExpDist_K<- DistHetero(H)

## -----------------------------------------------------------------------------
ObsDist_X<-FreqAlleleShare(AS)
ExpDist_X<-DistAlleleShare(e)

## -----------------------------------------------------------------------------
df_K <- ComposPare_K(h,ExpDist_K,trans = F)
df_X <- ComposPare_X(AS,ExpDist_X,trans = F)

## ----echo=FALSE, fig.height=4, fig.show='hold', fig.width=7-------------------

ggplot(df_K,aes(x=freq))+
  geom_histogram(aes(y=..density..,color=OvE,fill=OvE),alpha=0.5,binwidth = 1,position = "identity")+
  ggtitle("No. of Heterozyous loci")+
  xlab("No. of Heterozygous Loci(K)")+ylab("Density/Probability")+
  stat_function(data=ExpDist_K,mapping = aes(x=K,y=Density),fun = splinefun(ExpDist_K$K,ExpDist_K$Density),color="Red")

ggplot(df_X,aes(x=freq))+
  geom_histogram(aes(y=..density..,color=OvE,fill=OvE),alpha=0.5,binwidth = 1,position = "identity")+
  ggtitle("No. of Shared Alleles")+
  xlab("No. of Shared Alleles(X)")+ylab("Density/Probability")+
  stat_function(data=ExpDist_X,mapping = aes(x=X,y=Density),fun = splinefun(ExpDist_X$X,ExpDist_X$Density),color="Red")


