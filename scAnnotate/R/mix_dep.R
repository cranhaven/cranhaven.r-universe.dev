#' @title mix_dep
#'
#' @description annotate the cell by mixture model with nonparametric depth function
#'
#' @param train A data frame of cell type label in the first column and a gene expression matrix where each row is a cell and each column is a gene from training data
#' @param test A data matrix where each row is a cell and each column is a gene from test data
#' @param cellnames names of each cell population
#'
#' @return A vector contain annotate cell type labels for test data
#'
#' @noRd
#'
mix_dep=function(train,test,cellnames){

  ##############FIT MODEL WITH TRAINING DATASET####################
  #output the probability and make prediction
  px.matrix=qjm(dat=train,test =test)
  nb.outp=pred_addtiny(px.matrix = px.matrix)

  labels = nb.outp$predict_label

  return(labels)
}
