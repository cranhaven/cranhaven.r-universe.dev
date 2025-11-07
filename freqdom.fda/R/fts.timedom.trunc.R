#' Truncate functional timedom object
#' 
#' @title Truncate functional timedom object
#' @export
#' @keywords internal
fts.timedom.trunc = function(A, lags){
  if(!is.fts.timedom(A))
    stop("A must be an object of class fts.timedom")
  multA = timedom(A$operators,A$lags)
  newA = timedom.trunc(multA, lags)
  fts.timedom(newA,basisX=A$basisX,basisY=A$basisY)
}
