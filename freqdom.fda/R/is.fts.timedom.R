inClass = function(X,cls){
  (length(oldClass(X)) > 0) && any(oldClass(X) == cls)
}

#' Checks if an object belongs to the class \code{\link{fts.timedom}}.
#
#' @title Checks if an object belongs to the class fts.timedom
#' @param X some object
#' @return \code{TRUE} if \code{X} is of type \code{\link{fts.timedom}}, \code{FALSE} otherwise
#' @seealso \code{\link{fts.freqdom}}, \code{\link{fts.timedom}}, \code{\link{is.fts.freqdom}}
#' @export 
#' @keywords classes
is.fts.timedom = function (X){
  inClass(X,'fts.timedom')
}

