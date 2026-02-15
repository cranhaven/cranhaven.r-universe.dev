#' A Function for  Assisting Supervised Clustering
#'
#' when perfoming a supervised clustering the user should run this function in order to get the best results.
#' @usage  top_supervised(me,TOP=1000,cluster_which,TRw=-1)
#' @param me the matrix of the gene exporessions, the olums should be the samples  and the colnames the sample names
#' the rownames should be the genes . at best the ENTEREZID
#' @param TOP the top genes to choose, default is 100.
#' @param cluster_which a dataframe with the supervised clustering arrangment of the samples. the dataframe should have the
#' sample names in the first column and the clustering in the secound column.
#' @param TRw the threshhold for excluding samples with silhouette width < TRw
#' @return a list. the first place is the expression matrix, the secound is the silhouette for each sample.
#' @export top_supervised
#' @examples
#'
#' 
#' library(org.Hs.eg.db)
#' data(rna)
#' cluster_which<-cbind(colnames(rna),c(rep(1,times=24),rep(2,times=24)))
#' me_x=rna
#' ## calculate best number of clusters and
#' res<-top_supervised(me_x,TOP = 100,cluster_which)
#' me_TOP=res[[1]]
#' number_of_k=2
#' groups_men=res[[2]]
#' me_x=me_TOP
#' colnames(me_x)
#' o_g<-Supervised_Cluster_Heatmap(groups_men = groups_men, gene_matrix=me_x,
#'                                method="PAMR",show_sil=TRUE,print_genes=TRUE,threshold = 0,
#'                                TOP = 100,GSE=FALSE,plot_mean_sil=FALSE,stats_clust=res[[2]],
#'                                samples_data = as.data.frame(groups_men[,1,drop=FALSE]))
#'                                

top_supervised<-function(me,TOP=1000,cluster_which,TRw=-1){
  dim(me)
  sd=as.data.frame(apply(me,1, function(x){sd(x)}))
  sd=as.data.frame(sd[order(sd[,1], decreasing = T), ,drop = FALSE])
  me_TOP=me[rownames(sd)[1:TOP], ]
  dim(me_TOP)
  dist_mat<-as.matrix(cluster::daisy(t(me_TOP)))
  cluster_which<-as.data.frame(cluster_which)
  cluster_which[,2]<-as.numeric(cluster_which[,2])
  sil<-sil_width(cluster_which,dist_mat)
  groups_men<-cbind(as.data.frame(cluster_which),sil)
  groups_men[,2]<-as.numeric(groups_men[,2])
  groups_men[,3]<-as.numeric(groups_men[,3])
  rownames(groups_men)<-groups_men[,1]
  colnames(groups_men)<-c("sample","cluster","sil_width")
  groups_men<-groups_men[order(groups_men$cluster),]
  groups_men<-groups_men[,-1]
  groups_men<-groups_men[groups_men[,2]>TRw, ]
  Exp=(me[, rownames(groups_men)])
  return(list(Exp,groups_men))
}
