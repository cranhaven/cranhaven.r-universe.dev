#' @title 
#' Named lists
#' 
#' @description 
#' Functions to construct, coerce and check for named lists. 
#' 
#' @param ...
#' Named objects. 
#' 
#' @param x
#' Object to be coerced or tested. 
#' 
#' @return 
#' A named list. 
#'  
#' @importFrom kimisc nlist
#' @export
#' 
#' @examples 
#' x <- nlist(x = 2, y = c("a", "b"))
#' is.nlist(x)
#' 
nlist <- 
function(...)
{
  x <- kimisc::nlist(...)
  if (is.empty(x)) {
    names(x) <- character(0)
  }
  x
}


#' @export
#' @rdname nlist
#' 
as.nlist <-
function(x, 
         ...)
{
  y <- as.list(x, ...)
  names(y) <- names(x)
  if (is.empty(y)) return(nlist())
  if (!is.nlist(y)) stop("cannot convert 'x' into a named list")
  y
}


#' @export
#' @rdname nlist
#' 
is.nlist <- 
function(x)
{
  is.list(x) && 
    !is.null(names(x)) && 
    "" %nin% names(x)
}
