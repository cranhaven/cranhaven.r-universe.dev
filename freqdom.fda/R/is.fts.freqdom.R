#' Checks if an object belongs to the class \code{\link{fts.freqdom}}.
#
#' @title Checks if an object belongs to the class fts.freqdom
#' @param X some object
#' @return \code{TRUE} if \code{X} is of type \code{\link{fts.freqdom}}, \code{FALSE} otherwise
#' @seealso \code{\link{fts.freqdom}}, \code{\link{fts.timedom}}, \code{\link{is.fts.timedom}}
#' @export 
#' @keywords classes
is.fts.freqdom = function (X){
  inClass(X,'fts.freqdom')
}
