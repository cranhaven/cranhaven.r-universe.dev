#' cluster the samples
#'
#' This function clusters the samples into x clusters.
#' @usage Groups_Sup(me_TOP, me, number_of_k,TRw)
#'
#' @param me_TOP the matrix with the n top genes, usually the  from
#' output of the function TopPAM
#' @param me the original expression matrix. (with genes in rows and samples in columns).
#' @param number_of_k the number of clusters
#' @param TRw threshold for the elemenation of the samples with a Silhouette width lower than TRw.
#'  Default value is -1.
#'
#' @examples
#' 
#' ## load data
#' library(org.Hs.eg.db)
#' data(rna)
#' me_x=rna
#' res<-AutoPipe::TopPAM(me_x,max_clusters = 8, TOP=100)
#' me_TOP=res[[1]]
#' number_of_k=res[[3]]
#' File_genes=Groups_Sup(me_TOP, me=me_x, number_of_k,TRw=-1)
#' groups_men=File_genes[[2]]
#' me_x=File_genes[[1]]
#' 
#' @export Groups_Sup

Groups_Sup=function(me_TOP, me, number_of_k,TRw=-1){
  number_of_k=number_of_k
  pamx <- cluster::pam(t(me_TOP), k=number_of_k)
  si <- cluster::silhouette(pamx)
  graphics::barplot(si[,3])
  me_TOP_s=me_TOP[,rownames(si[si[,3]>TRw, ])]
  dim(me_TOP_s)
  groups_men=as.data.frame(si[si[,3]>TRw, ])
  graphics::barplot(groups_men$sil_width)
  (rownames(groups_men) %in% colnames(me) )
  Exp=(me[, rownames(groups_men)])
  dim(Exp)
  return(list(Exp, groups_men))
}
