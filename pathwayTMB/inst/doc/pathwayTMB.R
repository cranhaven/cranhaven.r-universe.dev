## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require(pathwayTMB)

## ----echo = TRUE, results = 'hide',eval=FALSE---------------------------------
#  install.packages("pathwayTMB")
#  library(pathwayTMB)

## ----out.width=24,echo = TRUE-------------------------------------------------
#get the path of the mutation annotation file and samples' survival data
maf<-system.file("extdata","data_mutations_extended.txt",package = "pathwayTMB")
sur_path<-system.file("extdata","sur.csv",package = "pathwayTMB")
sur<-read.csv(sur_path,header=TRUE,row.names = 1)
#perform the function 'get_mut_matrix'
#mut_matrix<-get_mut_matrix(maffile=maf,is.TCGA=FALSE,mut_fre=0,nonsynonymous = TRUE,   cut_fisher.pval=1,cut_oddsRatio=1,sur=sur)
#view the first six lines of mutation matrix
head(mut_matrix)[1:6,1:6]

## ----echo =TRUE, results = 'hide',eval=FALSE----------------------------------
#  #calculate coding genes' length, filepath--the path of the GTF file
#  #get_gene_length(filepath)
#  

## ----echo =TRUE, results = 'hold'---------------------------------------------
#perform the function `get_PTMB`
#PTMB_matrix<-get_PTMB(freq_matrix=mut_matrix,genesmbol=genesmbol,gene_path=gene_path)
#show the first six lines of PTMB matrix
head(PTMB_matrix)[1:6,1:6]

## ----warning=FALSE------------------------------------------------------------
# filter the survival-related pathways
#set.seed(1)
#final_character<-get_final_signature(PTMB=PTMB_matrix,sur=sur)
#view the final_character
final_character

## ----fig.height=6, fig.width=8,warning=FALSE,results='hold'-------------------
#Drawing Kaplan Meier Survival Curves using the final survival-related PTMB.
plotKMcurves(t(PTMB_matrix[final_character,]),sur=sur,returnAll = FALSE,risk.table = TRUE)

## ----fig.height=6, fig.width=8,warning=FALSE,results='hold'-------------------
#a mutually exclusive co-occurrence chart showing the top 20 genes for mutation rates
gene_fre<-apply(mut_matrix,1,function(x){length(which(x!=0))/length(x)})
genes<-names(sort(gene_fre,decreasing = TRUE))[1:20]
plotMutInteract(freq_matrix=mut_matrix, genes=genes,returnAll = FALSE)
#pathways' mutually exclusive co-occurrence chart
plotMutInteract(freq_matrix=PTMB_matrix,genes=final_character,
                nShiftSymbols =0.3,returnAll = FALSE)

## ----fig.height=6, fig.width=8,warning=FALSE,results='hold'-------------------
#calculate the PTMB-related riskscore
riskscore<-plotKMcurves(t(PTMB_matrix[final_character,]),sur=sur,plots = FALSE)$risk_score
cut_off<-median(riskscore)
#draw an GenePathwayOncoplots
GenePathwayOncoplots(maffile=maf,freq_matrix =mut_matrix,risk_score=riskscore,    
cut_off=cut_off,final_character=final_character,gene_path = gene_path,removeNonMutated = FALSE)

## ----fig.height=6, fig.width=8, warning=FALSE, results='hold'-----------------
#get the path of samples' immunotherapy response data
res_path<- system.file("extdata","response.csv",package = "pathwayTMB")
response<-read.csv(res_path,header=TRUE,stringsAsFactors =FALSE,row.name=1)
plotROC(riskscore=riskscore,response=response,main="Objective Response",print.auc=TRUE,grid = TRUE)

