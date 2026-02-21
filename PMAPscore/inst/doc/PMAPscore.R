## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PMAPscore)

## ----echo = TRUE, results = 'hide',eval=FALSE---------------------------------
#  install.packages("PMAPscore")
#  library(PMAPscore)

## ----out.width=24-------------------------------------------------------------
#load the mutation annotation file
data("maf_data")
#perform the function 'get_mut_status'
mut_status<-get_mut_status(maf_data=maf_data,nonsynonymous = TRUE)
#view the first five lines of mut_status matrix
mut_status[1:5,1:5]

## ----results='hide'-----------------------------------------------------------
#Method of obtaining data
data(mut_status,gene_Ucox_res,gene_symbol_Entrez)
#calculate the pfs_score of single sample
pfs_score<-get_pfs_score(mut_status[,1:2],percent=0.03,gene_Ucox_res,gene_symbol_Entrez)
#view the first five lines of pfs_score matrix
pfs_score[1:5,1:2]

## ----warning=FALSE------------------------------------------------------------
#load pfs_score and survival data
data(pfs_score,sur)
# filter the survival-related pathways
final_signature<-get_final_signature(pfs_score,sur)
#view the final_character
final_signature

## ----warning=TRUE, paged.print=TRUE-------------------------------------------
#Load sample mutation data
data(mut_sam,gene_Ucox,symbol_Entrez,path_cox_data,sur,path_Ucox_mul,sig)
#Perform the function `get_sample_classification`
get_sam_cla(mut_sam,gene_Ucox,symbol_Entrez,path_cox_data,sur,path_Ucox_mul,sig,cut_off=-0.986)
#class_res

## ----fig.height=6, fig.width=8,warning=FALSE,results='hold'-------------------
#Load the data
data(km_data)
#Drawing Kaplan-Meier Survival Curves.
get_km_survival_curve(km_data,cut_point,TRAIN = TRUE,risk.table=TRUE)

## ----fig.height=6, fig.width=8, warning=FALSE, results='hold'-----------------
#Get the data of ROC curve
data(roc_data)
#Drawing ROC Curves
get_roc_curve(roc_data,print.auc=TRUE,main="Objective Response")

## ----fig.height=6, fig.width=8,warning=FALSE,results='hold'-------------------
#obtain the risksciore
data(km_data)
risk_score<-km_data$multiple_score
names(risk_score)<-rownames(km_data)
cut_off<-median(risk_score)
#load the data
data(final_signature,path_gene,mut_status,maffile)
#draw an GenePathwayOncoplots
get_Oncoplots(maffile,path_gene,mut_status,risk_score,cut_off,final_signature,"Gap junction")

## ----fig.height=6, fig.width=8,warning=FALSE,results='hold'-------------------
#Load the data
data(km_data,response)
#Drawing the histogram.
get_response_plot(km_data,response,cut_point,TRAIN=TRUE)

