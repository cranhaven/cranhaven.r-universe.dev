## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadjrSiCKLSNMF----------------------------------------------------------
library(jrSiCKLSNMF)

DataMatrices<-SimData$Xmatrices
cell_type<-SimData$cell_type
SimSickleJr<-CreateSickleJr(DataMatrices,names=list("RNA","ATAC"))
rm(DataMatrices,SimData)
SimSickleJr<-AddSickleJrMetadata(SimSickleJr,cell_type,"true_cell_type")
rm(cell_type)
set.seed(10)
SimSickleJr<-BuildKNNGraphLaplacians(SimSickleJr)
SimSickleJr<-SetLambdasandRowReg(SimSickleJr,lambdaWlist=list(10,50),lambdaH=500,rowReg="None")
SimSickleJr<-NormalizeCountMatrices(SimSickleJr)

## ----irlba--------------------------------------------------------------------
SimSickleJr<-DetermineDFromIRLBA(SimSickleJr)
SimSickleJr<-GenerateWmatricesandHmatrix(SimSickleJr,d=5)

## ----runningjrsickls----------------------------------------------------------
start.time<-Sys.time()
SimSickleJr<-RunjrSiCKLSNMF(SimSickleJr,rounds=200,differr=1e-6,minibatch=TRUE,random_W_updates=TRUE,batchsize=100,seed=8,minrounds = 200)
stop.time<-Sys.time()
stop.time-start.time

## ----minibatchplotdiagnostic--------------------------------------------------
MinibatchDiagnosticPlot(SimSickleJr)

## ----determineclustering------------------------------------------------------
SimSickleJr<-DetermineClusters(SimSickleJr,printclValid = FALSE)

## ----clustering---------------------------------------------------------------
SimSickleJr<-ClusterSickleJr(SimSickleJr,numclusts=4)

## ----UMAPplots----------------------------------------------------------------
SimSickleJr<-CalculateUMAPSickleJr(SimSickleJr)
#Plotting based off of cluster
SimSickleJr<-PlotSickleJrUMAP(SimSickleJr,title="K-means clusters")


## ----clustering2--------------------------------------------------------------
SimSickleJr<-ClusterSickleJr(SimSickleJr,numclusts=3)

## ----UMAPplots2---------------------------------------------------------------
#Plotting based off of cluster
SimSickleJr<-PlotSickleJrUMAP(SimSickleJr,title="K-means clusters")
#Plotting based off of true cell type metadata
SimSickleJr<-PlotSickleJrUMAP(SimSickleJr,colorbymetadata="true_cell_type",title="True Cell Types",legendname="True Cell Types")

## ----UMAPplotsWH--------------------------------------------------------------
SimSickleJr<-CalculateUMAPSickleJr(SimSickleJr,modality=1)
SimSickleJr<-PlotSickleJrUMAP(SimSickleJr,title="K-means clusters: RNA modality",
                 umap.modality="W1H")
SimSickleJr<-PlotSickleJrUMAP(SimSickleJr,colorbymetadata="true_cell_type",
                 title="True Cell Type: RNA modality",legendname="True Cell Types",
                 umap.modality="W1H")
SimSickleJr<-CalculateUMAPSickleJr(SimSickleJr,modality=2)
SimSickleJr<-PlotSickleJrUMAP(SimSickleJr,title="K-means clusters: ATAC modality",umap.modality="W2H")
SimSickleJr<-PlotSickleJrUMAP(SimSickleJr,colorbymetadata="true_cell_type",
                 title="True Cell Type:ATAC modality",legendname="True Cell Types",
                 umap.modality="W2H")

