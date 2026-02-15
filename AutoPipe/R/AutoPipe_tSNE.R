#' Implemented t-distributed stochastic neighbor embedding 
#'
#' This function is used to upload a table into R for further use in the AutoPipe
#'
#'
#' @usage AutoPipe_tSNE(me,perplexity=30,max_iter=500,groups_men)
#' 
#' @param me The path of the expression table
#' @param perplexity numeric; Perplexity parameter
#' @param max_iter integer; Number of iterations (default: 1000)
#' @param groups_men the data frame with the group clustering that the function Groups_Sup or top_supervised (2. place on the list) returns with
#' the data about each sample and its coressponding cluster.
#' @export AutoPipe_tSNE




AutoPipe_tSNE=function(me,perplexity=30,max_iter=500,groups_men){
  set.seed(5000)
  ana=Rtsne::Rtsne(t(me), check_duplicates = F, dim=3, perplexity=perplexity, max_iter=max_iter)
  ana=data.frame(ana$Y)
  rownames(ana)=colnames(me)
  ana$cluster=groups_men[rownames(ana), ]$cluster
  max=max(groups_men[rownames(ana), ]$cluster)
  ana$col="red"
  col<-RColorBrewer::brewer.pal(n=max,name = "Paired")
  for(i in 1:max){
  color=col[i]
  ana[ana$cluster==i, ]$col=color
  
  }
  
  
  plot(ana[,1:2], col=ana$col, pch=19, bty="n", main="t-distributed stochastic neighbor embedding")
  
}