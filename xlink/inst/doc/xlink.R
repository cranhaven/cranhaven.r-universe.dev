## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  library("devtools")
#  install_github("qiuanzhu/xlink")
#  

## ----eval=FALSE----------------------------------------------------------
#  library("xlink")
#  head(Rdata)
#  

## ---- echo=FALSE, results='asis'-----------------------------------------
library(xlink)
knitr::kable(head(Rdata))

## ----eval=FALSE----------------------------------------------------------
#  Covars<-c("Age","Smoking","Treatment")
#  SNPs<-c("snp_1","snp_2")
#  output<-xlink_fit(os="OS",ostime="OS_time",snps=SNPs,gender="gender",covars=Covars, option =list(type="XCI",MAF_v=0.05),model="survival",data = Rdata)
#  

## ----echo=FALSE, results='asis'------------------------------------------
Covars<-c("Age","Smoking","Treatment")
SNPs<-c("snp_1","snp_2")
output<-xlink_fit(os="OS",ostime="OS_time",snps=SNPs,gender="gender",covars=Covars, option =list(type="XCI",MAF_v=0.05),model="survival",data = Rdata)


## ----echo=FALSE, results='asis'------------------------------------------
knitr::kable(output[1]$snp_1$coefficients)


knitr::kable(output[1]$snp_1$loglik)


## ----eval=FALSE----------------------------------------------------------
#  Covars<-c("Age","Smoking","Treatment")
#  SNPs<-c("snp_1","snp_2")
#  output<-xlink_fit(os="OS",ostime="OS_time",snps=SNPs,gender="gender",covars=Covars, option =list(type="all",MAF_v=0.05),model="survival",data = Rdata)
#  

## ----echo=FALSE, results='asis'------------------------------------------
output<-xlink_fit(os="OS",ostime="OS_time",snps=SNPs,gender="gender",covars=Covars, option =list(MAF_v=0.1),model="survival",data = Rdata)


knitr::kable(output$snp_1$`XCI-E`$coefficients)

knitr::kable(output$snp_1$`XCI-E`$loglik)


## ----echo=FALSE, results='asis'------------------------------------------

knitr::kable(output$snp_1$`XCI`$coefficients)

knitr::kable(output$snp_1$`XCI`$loglik)


## ----echo=FALSE, results='asis'------------------------------------------

knitr::kable(output$snp_1$`XCI-S`$coefficients)

knitr::kable(output$snp_1$`XCI-S`$loglik)

knitr::kable(output$snp_1$`XCI-S`$Gamma , col.names ="Gamma")


## ----echo=FALSE, results='asis'------------------------------------------

knitr::kable(output$snp_1$`Best model by AIC`, col.names = "Best model by AIC" )



## ----eval=FALSE----------------------------------------------------------
#  Covars<-c("Age","Smoking","Treatment")
#  SNPs<-c("snp_1","snp_2","snp_3")
#  result<-xlink_fit(os="OS",ostime ="OS_time",snps=SNPs,gender ="gender",covars=Covars,
#                     option =list(type="all",MAF_v=0.05), model="survival", data = Rdata)
#  select_output(input=result,pv_thold=10^-5)
#  

## ----echo=FALSE, results='asis'------------------------------------------
Covars<-c("Age","Smoking","Treatment")
SNPs<-c("snp_1","snp_2","snp_3")
result<-xlink_fit(os="OS",ostime ="OS_time",snps=SNPs,gender ="gender",covars=Covars, 
                   option =list(type="all",MAF_v=0.05), model="survival", data = Rdata)


knitr::kable( select_output(input=result,pv_thold=10^-5) )


