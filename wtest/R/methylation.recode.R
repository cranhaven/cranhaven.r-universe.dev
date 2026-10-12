#' Recode Methylation Data
#' @description {Code a CpG variable into two levels (high and low) by the two-mean clustering method.}
#' @param data a data frame or matrix contains methylation data in the columns.
#' @examples
#' data(methylation)
#' data.recoded <- methylation.recode(methylation)
#' @export
#' @importFrom stats kmeans
#'

methylation.recode<-function(data){
  data.recode<-apply(data,2,cluster)
  return(data.recode)
}

