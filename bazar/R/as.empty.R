#' @title 
#' Convert to an empty object
#' 
#' @description 
#' Convert \code{x} to an empty object. 
#' 
#' @param x
#' An object. 
#' 
#' @param ...
#' Additional parameterS. 
#' 
#' @return 
#' An empty object
#' 
#' @seealso 
#' \code{\link{is.empty}} in this package. 
#' 
#' @export
#' 
#' @examples 
#' x <- c("a", "b", "c")
#' as.empty(x)
#' class(as.empty(x)) # still a character
#' 
#' x <- factor(LETTERS)
#' as.empty(x)        # levels are kept
#' class(as.empty(x)) # still a factor
#' 
#' x <- data.frame(x = 1:3, y = 2:4)
#' as.empty(x)
#' 
as.empty <-
function(x, 
         ...)
{
  UseMethod("as.empty")
}


#' @export
#' @rdname as.empty
#'
as.empty.default <-
function(x, 
         ...)
{
  x[0L]
}


#' @export
#' @rdname as.empty
#'
as.empty.data.frame <-
function(x, 
         ...)
{
  x[0L,]
}
