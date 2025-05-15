## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadjrSiCKLSNMF----------------------------------------------------------
library(jrSiCKLSNMF)

## ----createSickleJr-----------------------------------------------------------
SimData<-jrSiCKLSNMF::SimData
DataMatrices<-SimData$Xmatrices
cell_type<-SimData$cell_type
SimSickleJr<-CreateSickleJr(DataMatrices)
rm(DataMatrices,SimData)

## ----addmetadata--------------------------------------------------------------
SimSickleJr<-AddSickleJrMetadata(SimSickleJr,cell_type,"true_cell_type")
rm(cell_type)

## ----graphlaplacian-----------------------------------------------------------
set.seed(10)
SimSickleJr<-BuildKNNGraphLaplacians(SimSickleJr)

## ----setlambdas---------------------------------------------------------------
SimSickleJr<-SetLambdasandRowReg(SimSickleJr,lambdaWlist=list(10,50),lambdaH=500,rowReg="None")

## ----Normalize----------------------------------------------------------------
SimSickleJr<-NormalizeCountMatrices(SimSickleJr)

## ----determineD---------------------------------------------------------------
SimSickleJr<-PlotLossvsLatentFactors(SimSickleJr,d_vector=4:6,parallel = FALSE,rounds=25)

## ----initializeWH-------------------------------------------------------------
SimSickleJr<-GenerateWmatricesandHmatrix(SimSickleJr,d=10)

## ----usepreviouslycalculated--------------------------------------------------
SimSickleJr<-SetWandHfromWHinitials(SimSickleJr,d=5)

## ----runningjrsickls----------------------------------------------------------
start.time<-Sys.time()
SimSickleJr<-RunjrSiCKLSNMF(SimSickleJr,rounds=1000,differr=1e-5,minibatch=FALSE)
stop.time<-Sys.time()
(time<-stop.time-start.time)

## ----clustering---------------------------------------------------------------
SimSickleJr<-DetermineClusters(SimSickleJr,printclValid=FALSE)
SimSickleJr<-ClusterSickleJr(SimSickleJr,numclusts=3)

## ----UMAPplots----------------------------------------------------------------
SimSickleJr<-CalculateUMAPSickleJr(SimSickleJr)
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

