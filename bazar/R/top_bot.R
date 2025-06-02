#' @title 
#' Top or bottom element of an object
#' 
#' @description 
#' \code{top(x)} is an alias for \code{head(x, 1L)}. 
#' \code{bot(x)} is an alias for \code{tail(x, 1L)}. 
#' 
#' @param x
#' an object. 
#' 
#' @return 
#' An object (usually) like \code{x} but generally smaller. 
#' 
#' @seealso 
#' \code{\link[utils]{head}} and \code{\link[utils]{head}} from package 
#' \pkg{utils}
#' 
#' @importFrom utils head
#' @export
#'
top <-
function(x)
{
  head(x, 1L)
}


#' @importFrom utils tail
#' @export
#' @rdname top
#'
bot <-
function(x)
{
  tail(x, 1L)
}
