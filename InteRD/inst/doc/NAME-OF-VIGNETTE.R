## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  if (!require("devtools")) {
#    install.packages("devtools")
#  }
#  devtools::install_github("chencxxy28/InteRD")

## ----message=FALSE------------------------------------------------------------
#load
library(InteRD)

## ----message=FALSE------------------------------------------------------------
readRDSFromWeb <- function(ref) {
  readRDS(gzcon(url(ref)))
}
seger <- readRDSFromWeb("https://github.com/chencxxy28/Data/raw/main/data_InteRD/segerstolpe.rds")
baron <- readRDSFromWeb("https://github.com/chencxxy28/Data/raw/main/data_InteRD/baron.rds")
xin<-readRDSFromWeb("https://github.com/chencxxy28/Data/raw/main/data_InteRD/Xin_nonD.rds")

## ---- message=FALSE-----------------------------------------------------------
set.seed(1234567)
pseudo.seger<-generateBulk(seger[["sc.eset.qc"]], ct.varname = "cluster", sample = "sample", ct.sub = c("alpha","beta","delta","gamma"), nbulk = 40, low_s = 0.3, upp_s = 0.7)
truep<-pseudo.seger$true_p[complete.cases(pseudo.seger$true_p),]

## ---- message=FALSE,eval=FALSE------------------------------------------------
#  set.seed(1234567)
#  library(SCDC)
#  ##ensemble of multiple reference sets
#  #resuts based on SCDC
#    pancreas.sc <- list(baron = baron$sc.eset.qc,
#                        xin   = xin
#    )
#    SCDC_results<-SCDC_ENSEMBLE(bulk.eset = pseudo.seger$pseudo_eset, sc.eset.list = pancreas.sc, ct.varname = "cluster",
#                                  sample = "sample", weight.basis =TRUE,truep = truep, ct.sub =  c("alpha","beta","delta","gamma"), search.length = 0.02,grid.search=TRUE)
#    comb<-SCDC_results$prop.only
#    weight_matrix<-SCDC_results$w_table["mAD_Y",1:2]
#    SCDC_ENSEMBLE_MAD<-SCDC:::wt_prop(weight_matrix,comb)
#  
#    # saveRDS(SCDC_ENSEMBLE_MAD,"/Users/chixiang.chen/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/postdoc/postdoc/deconvolution/ref_based_rd/data_InteRD/SCDC_ENSEMBLE_MAD_seger.rds")
#    # saveRDS(comb,"/Users/chixiang.chen/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/postdoc/postdoc/deconvolution/ref_based_rd/data_InteRD/comb_seger.rds")

## ---- message=FALSE-----------------------------------------------------------
#results based on InteRD1
SCDC_ENSEMBLE_MAD<-readRDSFromWeb("https://github.com/chencxxy28/Data/raw/main/data_InteRD/SCDC_ENSEMBLE_MAD_seger.rds")
comb<-readRDSFromWeb("https://github.com/chencxxy28/Data/raw/main/data_InteRD/comb_seger.rds")
  list_marker<-readRDSFromWeb("https://github.com/chencxxy28/Data/raw/main/data_InteRD/list_markerbaron20.rds") #get markers selected from xin et al (2016)
  lambda_option<-c(0,0.01,0.05,0.1,1,5,100)
  cell_type_unique<-c("alpha","beta","delta","gamma")
  InteRD1.output<-InteRD1(bulk.data =pseudo.seger$pseudo_eset,list_marker,cell_type_unique,comb_used=comb,lambda_option)
  InteRD1<-InteRD.predict.prop(InteRD.output=InteRD1.output)

## ---- message=FALSE-----------------------------------------------------------
evaluate(SCDC_ENSEMBLE_MAD,pseudo.seger$true_p)$all.eva
evaluate(InteRD1,pseudo.seger$true_p)$all.eva

## ---- message=FALSE-----------------------------------------------------------
ave_est = pop.ct.prop.scRNA(scRNA=seger[["sc.eset.qc"]],cell_type_unique=cell_type_unique)$pop.ct.prop
ave_sd = pop.ct.prop.scRNA(scRNA=seger[["sc.eset.qc"]],cell_type_unique=cell_type_unique)$pop.ct.sd
lambda_option<-c(0,seq(from=1,to=20,length=4),seq(from=30,to=100,length=4),200,500,1000000^2)
InteRD2.output<-InteRD2(bulk.data=pseudo.seger$pseudo_eset,list_marker,cell_type_unique,comb_sampled=InteRD1,ave_est,ave_sd,lambda_option=lambda_option)
InteRD2<-InteRD.predict.prop(InteRD.output=InteRD2.output)

## ---- message=FALSE-----------------------------------------------------------
ref_free.output<-Ref_free(bulk.data=pseudo.seger$pseudo_eset,list_marker=list_marker,cell_type_unique=cell_type_unique)
reffree<-InteRD.predict.prop(InteRD.output=ref_free.output)

## ---- message=FALSE-----------------------------------------------------------
evaluate(SCDC_ENSEMBLE_MAD,pseudo.seger$true_p)$all.eva
evaluate(reffree,pseudo.seger$true_p)$all.eva
evaluate(InteRD1,pseudo.seger$true_p)$all.eva
evaluate(InteRD2,pseudo.seger$true_p)$all.eva

## ---- message=FALSE,eval=FALSE------------------------------------------------
#  set.seed(1234567)
#  pseudo.baron<-generateBulk(baron[["sc.eset.qc"]], ct.varname = "cluster", sample = "sample", ct.sub = c("alpha","beta","delta","gamma"), nbulk = 40, low_s = 0.3, upp_s = 0.7)
#  truep<-pseudo.baron$true_p[complete.cases(pseudo.baron$true_p),]

## ---- message=FALSE,results='hide',eval=FALSE---------------------------------
#  set.seed(1234567)
#  ##ensemble of multiple reference sets
#  #resuts based on SCDC
#    pancreas.sc <- list(seger = seger$sc.eset.qc,
#                        xin   = xin
#    )
#    SCDC_results<-SCDC_ENSEMBLE(bulk.eset = pseudo.baron$pseudo_eset, sc.eset.list = pancreas.sc, ct.varname = "cluster",
#                                  sample = "sample", weight.basis =TRUE,truep = truep, ct.sub =  c("alpha","beta","delta","gamma"), search.length = 0.02,grid.search=TRUE)
#    comb<-SCDC_results$prop.only
#    weight_matrix<-SCDC_results$w_table["mAD_Y",1:2]
#    SCDC_ENSEMBLE_MAD<-SCDC:::wt_prop(weight_matrix,comb)
#  
#    # saveRDS(SCDC_ENSEMBLE_MAD,"/Users/chixiang.chen/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/postdoc/postdoc/deconvolution/ref_based_rd/data_InteRD/SCDC_ENSEMBLE_MAD_baron.rds")
#      # saveRDS(comb,"/Users/chixiang.chen/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/postdoc/postdoc/deconvolution/ref_based_rd/data_InteRD/comb_baron.rds")

## ---- message=FALSE,eval=FALSE------------------------------------------------
#  #results based on InteRD1
#  SCDC_ENSEMBLE_MAD<-readRDSFromWeb("https://github.com/chencxxy28/Data/raw/main/data_InteRD/SCDC_ENSEMBLE_MAD_baron.rds")
#  comb<-readRDSFromWeb("https://github.com/chencxxy28/Data/raw/main/data_InteRD/comb_baron.rds")
#    list_marker<-readRDSFromWeb("https://github.com/chencxxy28/Data/raw/main/data_InteRD/list_markerbaron20.rds") #get markers selected from xin et al (2016)
#    lambda_option<-c(0,0.01,0.05,0.1,1,5,100)
#    cell_type_unique<-c("alpha","beta","delta","gamma")
#    InteRD1.output<-InteRD1(bulk.data =pseudo.baron$pseudo_eset,list_marker,cell_type_unique,comb_used=comb,lambda_option)
#    InteRD1<-InteRD.predict.prop(InteRD.output=InteRD1.output)

## ---- message=FALSE,eval=FALSE------------------------------------------------
#  ave_est = pop.ct.prop.scRNA(scRNA=baron[["sc.eset.qc"]],cell_type_unique=cell_type_unique)$pop.ct.prop
#  ave_sd = pop.ct.prop.scRNA(scRNA=baron[["sc.eset.qc"]],cell_type_unique=cell_type_unique)$pop.ct.sd
#  lambda_option<-c(0,seq(from=1,to=20,length=4),seq(from=30,to=100,length=4),200,500,1000000^2)
#  InteRD2.output<-InteRD2(bulk.data=pseudo.baron$pseudo_eset,list_marker,cell_type_unique,comb_sampled=InteRD1,ave_est,ave_sd,lambda_option=lambda_option)
#  InteRD2<-InteRD.predict.prop(InteRD.output=InteRD2.output)

## ---- message=FALSE,eval=FALSE------------------------------------------------
#  evaluate(SCDC_ENSEMBLE_MAD,pseudo.baron$true_p)$all.eva
#  evaluate(InteRD1,pseudo.baron$true_p)$all.eva
#  evaluate(InteRD2,pseudo.baron$true_p)$all.eva

