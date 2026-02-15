#' Unsupervised Clustering
#'
#' A function for unsupervised Clustering of the data
#'
#' @usage UnSuperClassifier(data,clinical_data=NULL,thr=2,TOP_Cluster=150,TOP=100)
#' @export UnSuperClassifier
#'
#' @param data the data for the clustering. Data should be in the following format: samples in columns and
#' the genes in the rows (colnames and rownames accordingly). The rownames should be Entrez ID in order to
#' plot a gene set enrichment analysis.
#' @param clinical_data the clinical data provided by the user to plot under the heatmap. it will be
#'  plotted only if show_clin is TRUE. Default value is NULL. see details for format.
#' @param TOP_Cluster numeric; Number of genes in each cluster.
#' @param TOP numeric; the number of the TOP genes to take from the gene exoression matrix see TopPAM TOP.
#' @param thr The threshold for the PAMR algorithm default is 2.
#' @details sample data should be a data.frame with the sample names
#'  as rownames and the clinical triats as columns.
#'  each trait must be a numeric variable.
#'  @return the function is an autated Pipeline for clustering it plot cluster analysis for the geneset
#' @import  graphics
UnSuperClassifier<-function(data,clinical_data=NULL,thr=2,TOP_Cluster=150,TOP=100){
  res<-AutoPipe::TopPAM(data,max_clusters = 8, TOP=TOP)
  me_TOP=res[[1]]
  dim(me_TOP)
  #-> Wähle Nr of Cluster
  number_of_k=res[[3]]
  File_genes=Groups_Sup(me_TOP, me=data, number_of_k,TRw=-1)
  groups_men=File_genes[[2]]
  if(is.null(clinical_data)){
    o_g<-Supervised_Cluster_Heatmap(groups_men = groups_men, gene_matrix=File_genes[[1]],TOP_Cluster=TOP_Cluster,
                                    method="PAMR",show_sil=TRUE,threshold = thr
                                    ,print_genes=T,TOP = TOP,GSE=T,plot_mean_sil=T,stats_clust=res[[2]])
  }
  else{
    o_g<-Supervised_Cluster_Heatmap(groups_men = groups_men, gene_matrix=File_genes[[1]],threshold=thr,TOP_Cluster=TOP_Cluster,
                                    method="PAMR",show_clin =TRUE,show_sil=TRUE,samples_data = clinical_data
                                    ,print_genes=T,TOP = 1000,GSE=T,plot_mean_sil=T,stats_clust=res[[2]])

  }
}
