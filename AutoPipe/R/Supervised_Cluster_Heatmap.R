#' Produce a Heatmap using a Supervised clustering Algorithm
#'
#' This function produces a plot with a Heatmap using
#' a supervised clustering algorithm which the user choses.
#' with a the  mean Silhouette width plotted on the right top corner
#' and the Silhouette width for each sample on top.
#' On the right side of the plot the n highest and lowest scoring
#' genes for each cluster will added. And next to them the coressponding pathways
#' (see Details)
#'
#'
#' @usage Supervised_Cluster_Heatmap(groups_men, gene_matrix,
#' method="PAMR",TOP=1000,TOP_Cluster=150,
#' show_sil=FALSE,show_clin=FALSE,genes_to_print=5,
#' print_genes=FALSE,samples_data=NULL,colors="RdBu",
#' GSE=FALSE,topPaths=5,db="c2",plot_mean_sil=FALSE,stats_clust =NULL,threshold=2)
#' @param groups_men the data frame with the group clustering that the function Groups_Sup or top_supervised (2. place on the list) returns with
#' the data about each sample and its coressponding cluster.
#' @param gene_matrix the matrix of n selected genes that the function Groups_Sup returns
#' @param method the method to cluster of Clustering. The default is "PAMR" which uses the pamr library.
#' other methods are SAM and our own "EXReg" (see details)
#' @param TOP the number of the top genes to take. the default value is 1000.
#' @param show_sil a logical value that indicates if the function should show
#' the Silhouette width for each sample. Default is FALSE.
#' @param show_clin a logical value if TRUE the function will plot the clinical data provided
#'  by the user. Default value is FALSE.
#' @param genes_to_print the number of genes to print for each cluster. this function
#'  adds on the right side.
#' of the heatmap the n highest expressed genes and the n lowest expressed genes for each
#'  cluster. Default value is 5.
#' @param print_genes a logical value indicating if or not to plot the TOP genes for each
#'  cluster.Default value is FALSE.
#' @param  samples_data the clinical data provided by the user to plot under the heatmap. it will be
#'  plotted only if show_clin is TRUE. Default value is NULL. see details for format.
#' @param colors the colors for the Heatmap. The function RColorBrewer palletes.
#' @param GSE a logical variable that indicates wether to  plot thr Gene Set Enrichment Analysis
#' next to the heatmap. Default value is FALSE.
#' @param topPaths a numerical value that says how many pathways the Gene Set Enrichment
#' plots should contain fo each cluster. Default value is 5.
#' @param db a value for the database for the GSE to be used. Default value is "c1".
#' the paramater can one of the values: "c1","c2","c3",c4","c5","c6","c7","h". See the
#' broad institue GSE \href{http://software.broadinstitute.org/gsea/index.jsp}{GSE webpage} for further information in each dataset.
#' @param plot_mean_sil A logical value. if TRUE the function plots the mean of the
#' Silhouette width for each cluster number or gap statistic.
#' @param stats_clust A vector with the mean Silhouette widths or gap statistic for the number of clusters. The first
#' value should be for 2 Clusters. 2nd is for 3 clusters and so on.
#' @param threshold the threshhold for the pam analysis default is 2.
#' @param TOP_Cluster a numeric variable for the number of genes to include in the clusters. Default is 150.
#' @details sample data should be a data.frame with the sample names
#'  as rownames and the clinical triats as columns.
#'   each trait must be a numeric variable.
#'
#'
#' @examples
#'
#' ##load the org.Hs.eg Library
#' library(org.Hs.eg.db)
#' ## load data
#' data(rna)
#' me_x=rna
#' ## calculate best number of clusters and
#' res<-AutoPipe::TopPAM(me_x,max_clusters = 6, TOP=100)
#' me_TOP=res[[1]]
#' number_of_k=res[[3]]
#' File_genes=Groups_Sup(me_TOP, me=me_x, number_of_k,TRw=-1)
#' groups_men=File_genes[[2]]
#' me_x=File_genes[[1]]
#' o_g<-Supervised_Cluster_Heatmap(groups_men = groups_men, gene_matrix=me_x,
#'     method="PAMR",show_sil=TRUE,print_genes=TRUE,threshold=0,
#'     TOP = 100,GSE=FALSE,plot_mean_sil=TRUE,stats_clust=res[[2]])
#'
#' @import  graphics
#' @export Supervised_Cluster_Heatmap
Supervised_Cluster_Heatmap=function(groups_men, gene_matrix, method="PAMR",TOP=1000,TOP_Cluster=150,
                                     show_sil=FALSE,show_clin=FALSE,genes_to_print=5,
                                     print_genes=FALSE,samples_data=NULL,colors="RdBu",
                                     GSE=FALSE,topPaths=5,db="c2",plot_mean_sil=FALSE,stats_clust =NULL,threshold=2){
  cluster_files=supVisGenes(groups_men,gene_matrix=gene_matrix,method=method,TOP = TOP,threshold = threshold,TOP_Cluster=TOP_Cluster)
  ordert_genes<-cluster_files[[1]]
  genes_print_list<-cluster_files[[2]]
  sil_w <-if(show_sil){as.data.frame(groups_men[,c("cluster","sil_width"),drop=FALSE])}else{NULL}

  new_nchheatmap(ordert_genes, sil_width =sil_w
                 ,samples_data = samples_data,print_genes = print_genes,list_of_genes = genes_print_list
                 ,plot_mean_sil=plot_mean_sil,sil_mean =stats_clust,
                 genes_to_print=genes_to_print,col=colors,GSE=GSE,db=db,topPaths=topPaths)
  return(cluster_files)

}
