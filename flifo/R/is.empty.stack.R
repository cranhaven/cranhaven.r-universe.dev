#' @title 
#' Test emptyness of a stack
#' 
#' @description 
#' This method tests if a stack \code{x} is empty.
#' 
#' @param x
#' A stack. 
#' 
#' @return 
#' A logical, \code{TRUE} if \code{x} is empty. 
#' 
#' @seealso The generic function \code{\link[bazar]{is.empty}} 
#' in package \pkg{bazar}. 
#' 
#' @export
#' 
is.empty.stack <-
function(x)
{
  stopifnot(is.stack(x))
  length(x) == 0L
}


#' @importFrom bazar is.empty
#' @export
#' 
bazar::is.empty
