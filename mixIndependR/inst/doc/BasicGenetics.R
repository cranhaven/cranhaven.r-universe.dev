## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

## ----load packages, include=FALSE---------------------------------------------
library(mixIndependR)

## ----import-------------------------------------------------------------------
x <- mixexample

## ----allele freq, echo=TRUE---------------------------------------------------
p <- AlleleFreq(x,sep = "\\|")

## ----genotype freq, echo=TRUE-------------------------------------------------
G <- GenotypeFreq(x,sep = "\\|",expect = FALSE) 
G0 <- GenotypeFreq(x,sep = "\\|",expect = TRUE) 

## ----Heterozygosity, echo=TRUE------------------------------------------------
h <-Heterozygous(x,sep = "\\|") ####or Just use Heterozygous(x)

## ----Hetero Expect, echo=TRUE-------------------------------------------------
H <- RxpHetero(h,p,HWE=TRUE)

## ----AS-----------------------------------------------------------------------
AS<-AlleleShare(x,sep = "\\|",replacement = FALSE) 

## ----Allele Share Expect, message=FALSE, warning=FALSE------------------------
e <-RealProAlleleShare(AS)
e0<-ExpProAlleleShare(p)

## ----HWE----------------------------------------------------------------------

HWE_pvalue <-HWE.Chisq(G,G0,rescale.p = T,simulate.p.value = T,B=2000)

