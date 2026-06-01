#' @title pred_addtiny
#'
#' @description Annotate the cell type labels by combiner function based on Naive Bayes Classifier
#'
#' @param px.matrix A array contains probability for each cell (matrix) on each cell population (column) and each gene (row)
#'
#' @return A list contain annotate cell type labels and largest combiner function value.
#'
#' @noRd
pred_addtiny=function(px.matrix){
  votem.2 <- matrix(NA,nrow =dim(px.matrix)[3],ncol = ncol(px.matrix))
  colnames(votem.2)=colnames(px.matrix)
  #add all probability for posterior probabilities
  votem.2=t(colSums(log(px.matrix+5e-324),na.rm = TRUE))
  #assign to the cell type which has the largest probability
  max.ind=apply(votem.2,1,FUN = which.max)
  max.ind=as.numeric(max.ind)
  output=colnames(votem.2)[max.ind]
  max.value=apply(votem.2,1,FUN = max)
  outp.list=list(predict_label=output,
                 probability_largest=max.value)
  return(outp.list)
}
