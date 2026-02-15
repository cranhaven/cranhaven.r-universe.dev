# AUtomated TranscriptOme Classifier Pipeline (AutoPipe): An R-Package for Comprehensive Transcriptome Analysis


Transcriptomic is the large-scale identification of gene expression across multiple samples.
Gene expression mirrored functional aspects and included important information
about biological functions and pathway activation. Their analysis can either
uncover molecular functions on the one side and improve classification of large
cohorts for improved clinical understanding on the other side. This tool aimed to design a
standard-pipeline to integrate classification and functional aspect and
generate a visual output to integrate transcriptomic data, clinical
information and Gene Set Enrichment Analysis for functional aspects. 

The pipeline was designed to integrate following aspects:

Reproducibility: Analysis needs to be easily reproduced by external researchers.

Easy-to-Use: The pipeline was designed to be user-friendly and applicable for non-expert users.

Compatible: The pipeline should be feasible for array based transcriptomic data as well as RNA sequencing outputs. For further clinical interpretation, external traits need to be easily integrated and included in the analysis.






## How to install the package from GitHub

Install with devtools
```
install.packages("devtools")
library(devtools)
install_github("falafel19/AutoPipe")
```
## Unsupervised Cluster Analysis

### Description

A function for unsupervised Clustering of the data


```
#Load data with Gene ENTREZ in rownames and samples in colnames
data(y)
dim(data)

#Optional: Read in clinical Infos with samples in rownames

UnSuperClassifier(data,clinical_data=NULL,thr=2)

```


## Produce a Heatmap using a Supervised Clustering Algorithm
### Description

This function produces a plot with a Heatmap using a supervised clustering algorithm which the user choses. with a the mean Silhouette width plotted on the right top corner and the Silhouette width for each sample on top. On the right side of the plot the n highest and lowest scoring genes for each cluster will added. And next to them the coressponding pathways (see Details)

```
##load the org.Hs.eg Library
library(org.Hs.eg.db)
#' ## load data
data(rna)
me=rna

## calculate best number of clusters 
res<-TopPAM(me, max_clusters = 8, TOP=1000)

me_TOP=res[[1]]
number_of_k=res[[3]]

## Compute top genes of each cluster, with "TRw" samples with a negative Silhouette widths could be cut-off

File_genes=Groups_Sup(me_TOP, me=me, number_of_k,TRw=-1)

groups_men=File_genes[[2]]
me_x=File_genes[[1]]

# groups_men contain informations of each sample and cluster, this could be adapted in case of a supervised analysis

o_g<-Supervised_Cluster_Heatmap(groups_men = groups_men, gene_matrix=me_x, method="PAMR",show_sil=TRUE,print_genes=TRUE, TOP = 1000,GSE=TRUE,plot_mean_sil=TRUE,sil_mean=res[[2]])

#Validate with Consensus Cluster or tSNE

cons_clust(me_x,max_clust=8, TOP=1000)
AutoPipe.tSNE(me=me_x)


```




### Authors

D. H. Heiland & K. Daka, Translational Research Group, Medcal-Center Freiburg, University of Freiburg


