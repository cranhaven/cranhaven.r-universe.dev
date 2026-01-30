#########1#########2#########3#########4#########5#########6#########7#########8
#' Obtain a Dataset from a Formula 
#'
#' Given a formula, a dataset and a subset, retrieve the dataset that fulfills
#' the formula and subset.
#'
#' @param formula A formula
#' @param data A dataset
#' @param subset Either a logical vector or a vector of indices of the rows
#' to be returned. If NULL (default), all rows are returned.
#' @return The dataset in \code{data} as a data table with variables as 
#' specified in \code{formula} and rows as specified by \code{subset}.
#' @examples
#' dataSet(mpg~.-disp,mtcars,10:20)
#' @import data.table
#' @export
################################################################################
dataSet<-function(formula,data,subset=NULL) {
  if (!inherits(formula,"formula")) stop("Formula must be a formula")
  if (!inherits(data,"data.frame")) stop("Data must be a data frame")
  response=as.character(formula[[2]])
  cols=c(response,attributes(terms(formula,data=data))$term.labels)
  data=as.data.table(data)
  if (is.null(subset)) data[,cols,with=FALSE] else data[subset,cols,with=FALSE]
}
