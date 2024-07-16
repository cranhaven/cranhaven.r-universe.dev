## ----message=FALSE, echo=FALSE, warning=FALSE---------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7.71,
  dpi=100,
  results = "asis"
)

## -----------------------------------------------------------------------------
library(visualpred)
dataf<-breastwisconsin1
listconti=c( "clump_thickness","uniformity_of_cell_shape",
             "marginal_adhesion","bare_nuclei",
             "bland_chromatin", "normal_nucleoli", "mitosis")
listclass=c("")
vardep="classes"
result<-famdcontour(dataf=dataf,listconti,listclass,vardep,title="FAMD Plots",selec=1)


## ----echo=F-------------------------------------------------------------------
result[[1]]

## ----echo=F-------------------------------------------------------------------
result[[2]]

## ----echo=F-------------------------------------------------------------------
result[3]

## ----echo=F-------------------------------------------------------------------
result[[4]]

## ----echo=F-------------------------------------------------------------------
result[5]

## ----echo=F-------------------------------------------------------------------
result[6]

## -----------------------------------------------------------------------------
library(visualpred)
dataf<-pima
listconti<-c("pregnant", "glucose", "pressure", "triceps", "insulin", "bodymass", 
             "pedigree", "age")
listclass<-c("")
vardep<-"diabetes"

resultfamd<-famdcontour(dataf=pima,listconti,listclass,vardep,title="FAMD",selec=1,
alpha1=0.7,alpha2=0.7,proba="",modelo="glm")
resultmca<-mcacontour(dataf=pima,listconti,listclass,vardep,title="MCA",proba="",
modelo="glm",selec=1,alpha1=0.7,alpha2=0.7)


## ----echo=F-------------------------------------------------------------------
resultfamd[[4]]

## ----echo=F-------------------------------------------------------------------
resultmca[[4]]

## -----------------------------------------------------------------------------
library(visualpred)
dataf<-na.omit(Hmda)
listconti<-c("dir", "hir", "lvr", "ccs", "mcs", "uria")
listclass<-c("pbcr", "dmi", "self", "single", "condominium", "black")
vardep<-c("deny")
resultfamd<-famdcontour(dataf=Hmda,listconti,listclass,vardep,title="FAMD",selec=1,
alpha1=0.7,alpha2=0.7,proba="",modelo="glm")
resultmca<-mcacontour(dataf=Hmda,listconti,listclass,vardep,title="MCA",proba="",
modelo="glm",selec=1,alpha1=0.7,alpha2=0.7)

## ----echo=F-------------------------------------------------------------------
resultfamd[[4]]
resultmca[[4]]

