#' @title 
#' Test emptyness
#' 
#' @description 
#' These methods test if an object \code{x} is empty. 
#' 
#' @param x
#' An object to be tested. 
#' 
#' @return 
#' \code{TRUE} if \code{x} is empty, \code{FALSE} otherwise. 
#' 
#' @seealso 
#' \code{\link[bazar]{as.empty}} in this package. 
#' 
#' @export
#' 
#' @examples 
#' is.empty(4)
#' is.empty(c())
#' is.empty(new.env())
#' is.empty(character(0))
#' is.empty(list())
#' is.empty(integer(0))
#' is.empty(data.frame())
#' 
is.empty <- 
function(x)
{
  UseMethod("is.empty")
}


#' @export
#' @rdname is.empty
#' 
is.empty.default <- 
function(x)
{
  length(x)==0L
}


#' @export
#' @rdname is.empty
#' 
is.empty.data.frame <- 
function(x)
{
  nrow(x) == 0L #|| ncol(x) == 0L
}
