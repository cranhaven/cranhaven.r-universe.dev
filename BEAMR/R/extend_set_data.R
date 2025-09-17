#' Extend set definition data with genes on the same row separated by commas, semicolons, slashes, etc
#'
#' @param set.data A data frame with set definition data.
#' @param sep Punctuation to split on.
#'
#' @returns A data frame.
#' @export
#'
#' @examples
#' data(setdat)
#' extend_set_data(setdat, sep=",")
extend_set_data=function(set.data,sep)

{
  set.id=strsplit(set.data$set.id,split=sep,fixed=TRUE)
  set.id=lapply(set.id,unique)
  k=lapply(set.id,length)
  k=unlist(k)
  row.id=rep(set.data$row.id,k)
  mtx.id=rep(set.data$mtx.id,k)
  set.id=unlist(set.id)
  res=cbind.data.frame(set.id=set.id,
                       mtx.id=mtx.id,
                       row.id=row.id)
  return(res)

}
