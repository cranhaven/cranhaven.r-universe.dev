## ----style, echo=FALSE, results="asis", message=FALSE-------------------------
knitr::opts_chunk$set(tidy = FALSE,
                      warning = FALSE,
                      message = FALSE)

## -----------------------------------------------------------------------------
library(ICDS)
# obtain the expression profile data
exp_data<-GetExampleData("exp_data")
#view first six rows and six colmns of data
exp_data[1:6, 1:6]

## ----eval=TRUE----------------------------------------------------------------
#obtain the labels of the samples of the expression profile, the label vector is a vector of 0/1s,
# 0 represents the case sample and 1 represents the control sample
label1<-GetExampleData("label1")
#view first ten label
label1[1:10]

## -----------------------------------------------------------------------------
#calculate p-values or corrected p-values for each gene

exp.p<-getExpp(exp_data,label = label1,p.adjust = FALSE)
label2<-GetExampleData("label2")
meth_data<-GetExampleData("meth_data")
meth.p<-getMethp(meth_data,label = label2,p.adjust = FALSE)
exp.p[1:10,]


## -----------------------------------------------------------------------------
#obtain Copy number variation data
cnv_data<-GetExampleData("cnv_data")
#obtion amplified genes
amp_gene<-GetExampleData("amp_gene")
#obtion deleted genes
del_gene<-GetExampleData(("del_gene"))


## -----------------------------------------------------------------------------
#calculate p-values or corrected p-values for each gene
cnv.p<-getCnvp(exp_data,cnv_data,amp_gene,del_gene,p.adjust=FALSE,method="fdr")
cnv.p[1:10,]

## -----------------------------------------------------------------------------
#obtain the p-values of expression profile data,methylation profile data and Copy number variation data
exp.p<-GetExampleData("exp.p")
meth.p<-GetExampleData("meth.p")
cnv.p<-GetExampleData("cnv.p")


## -----------------------------------------------------------------------------

#calculate z-scores for p-values of each kind of data
zexp<-coverp2zscore(exp.p)
zmeth<-coverp2zscore(meth.p)
zcnv<-coverp2zscore(cnv.p)
#combine two kinds of p-values,then,calculate z-score for them
zz<-combinep_two(exp.p,meth.p)
#combine three kinds of p-values,then,calculate z-score for them
zzz<-combinep_three(exp.p,meth.p,cnv.p)
zzz[1:6,]

## -----------------------------------------------------------------------------
#obtain z-score of each gene
zzz<-GetExampleData("zzz")
zzz[1:10,]


## ----eval= FALSE--------------------------------------------------------------
#  require(graphite)
#  zz<-GetExampleData("zzz")
#  #subpathdata<-FindSubPath(zz) #only show
#  

## -----------------------------------------------------------------------------
subpathdata<-GetExampleData("subpathdata")
keysubpathways<-opt_subpath(subpathdata,zz,overlap=0.6)
head(keysubpathways)

## -----------------------------------------------------------------------------
keysubpathways<-Permutation(keysubpathways,zz,nperm1=100,method1=TRUE,nperm2=100,method2=FALSE)
head(keysubpathways)

## -----------------------------------------------------------------------------
require(graphite)
require(org.Hs.eg.db)
subpID<-unlist(strsplit("ACSS1/ALDH3B2/ADH1B/ADH1A/ALDH2/DLAT/ACSS2","/"))
pathway.name="Glycolysis / Gluconeogenesis"
zzz<- GetExampleData("zzz")
PlotSubpathway(subpID=subpID,pathway.name=pathway.name,zz=zzz)

