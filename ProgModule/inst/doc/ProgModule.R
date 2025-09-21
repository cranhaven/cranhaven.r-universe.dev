## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = TRUE, results = 'hide',eval=FALSE---------------------------------
#  install.packages("ProgModule")

## ----setup--------------------------------------------------------------------
library(ProgModule)

## ----pressure, echo=FALSE,  out.width = '80%'---------------------------------
knitr::include_graphics("../inst/flowchart.png")

## -----------------------------------------------------------------------------
#load the mutation annotation file
maf<-system.file("extdata","maffile.maf",package = "ProgModule")
maf_data<-read.delim(maf)
mutvariant<-maf_data[,c("Hugo_Symbol","Tumor_Sample_Barcode","Variant_Classification")]
#perform the function 'get_mut_status'
mut_status<-get_mut_status(mutvariant=mutvariant,nonsynonymous = TRUE)
#view the first five lines of mut_status matrix
mut_status[1:5,1:5]

## -----------------------------------------------------------------------------
#load mutation matrix and PPI network
data(mut_status,subnet)
# find the local network of each gene
localnetwork<-get_local_network(network=subnet,freq_matrix=mut_status,max.size=500)

## -----------------------------------------------------------------------------
#load the data
data(mut_status,net,module,univarCox_result)
sur<-system.file("extdata","sur.csv",package = "ProgModule")
sur<-read.delim(sur,sep=",",header=TRUE,row.names = 1)
#Calculate the PRMEM score of module
mutuallyexclusivemodule<-get_mutual_module(module=module,net=net,freq_matrix=mut_status,sur=sur,module_sig="risk",univarCox_result,rate=0.05)
#view the scores of the modules
mutuallyexclusivemodule

## -----------------------------------------------------------------------------
#load the data
data(local_network,mut_status,subnet)
sur<-system.file("extdata","sur.csv",package = "ProgModule")
sur<-read.delim(sur,sep=",",header=TRUE,row.names = 1)
canonical_drivers<-system.file("extdata","canonical_drivers.txt",package = "ProgModule")
seed_gene<-read.table(canonical_drivers,header=FALSE)
gene<-intersect(seed_gene[,1],names(local_network))
#Get candidate modules
candidatemodule<-get_candidate_module(local_network=local_network,network=subnet,freq_matrix=mut_status,sur=sur,seed=gene,max.size=200,rate=0.05)
#View the top 10 candidate modules
candidatemodule[["module_set"]][1:5]

## ----fig.height=6, fig.width=8,warning=FALSE,results='hold'-------------------
#Load the data
data(mut_status,final_candidate_module)
sur<-system.file("extdata","sur.csv",package="ProgModule")
sur<-read.delim(sur,sep=",",header=TRUE,row.names=1)
#Drawing Kaplan-Meier Survival Curves.
get_mut_survivalresult(module=final_candidate_module,freq_matrix=mut_status,sur)

## ----fig.height=6,fig.width=8,warning=FALSE,results='hide'--------------------
#Load the data
data(plotMutInteract_moduledata,plotMutInteract_mutdata)
#Drawing an plotMutInteract of genes
get_plotMutInteract(genes=unique(unlist(plotMutInteract_moduledata[1:4])),freq_matrix=plotMutInteract_mutdata)
#Drawing an plotMutInteract of modules
get_plotMutInteract(module=plotMutInteract_moduledata,freq_matrix=plotMutInteract_mutdata,nShiftSymbols=0)

## ----fig.height=6,fig.width=8,warning=FALSE,results='hide'--------------------
#obtain the modules
data(final_candidate_module)
#load the maf data
maffile<-system.file("extdata","maffile.maf",package="ProgModule")
#Drawing an oncoplot
get_oncoplots(maf=maffile,genes=final_candidate_module[[3]])

## ----fig.height=6,fig.width=8,warning=FALSE,results='hide'--------------------
#load the maf data
data(maf_data)
#Drawing an lollipopPlot of TP53
get_lollipopPlot(maf=maf_data,gene="TP53")

