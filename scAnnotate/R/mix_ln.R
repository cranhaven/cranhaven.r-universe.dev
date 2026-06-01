#' @title mix_ln
#'
#' @description annotate the cell by mixture model with normal distribution assumption
#'
#' @param train A data frame of cell type label in the first column and a gene expression matrix where each row is a cell and each column is a gene from training data
#' @param test A data matrix where each row is a cell and each column is a gene from test data
#' @param cellnames names of each cell population
#'
#' @return A vector contain annotate cell type labels for test data
#'
#' @noRd
mix_ln=function(train,test,cellnames){

  ##############FIT MODEL WITH TRAINING DATASET####################
  #1) estimated from training dataset
  #mix model parameter for each celltype
  mix.d=mix_ln_pt(dat = train,cellnames=cellnames)

  #2) output the probability
  px.matrix=probability_ln(test=test,mix.d = mix.d)
  #make preidiction
  nb.outp=pred_addtiny(px.matrix = px.matrix)

  labels = nb.outp$predict_label

  return(labels)
}
