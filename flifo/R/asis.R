
#' @importFrom bazar is.wholenumber
#' @export
#' @rdname fifo
#' 
is.stack <-
function(x)
{
  ml <- attr(x, "max_length")
  si <- attr(x, "sizes")
  ms <- attr(x, "max_size")
  
  !is.null(ml) && 
    (bazar::is.wholenumber(ml) || is.infinite(ml)) &&
    !is.null(si) && 
    is.numeric(si) &&
    all(si >= 0) && 
    ml >= length(si) - 1L && 
    !is.null(ms) && 
    is.numeric(ms) && 
    ms >= sum(si) && 
    is.list(x) && 
    inherits(x, "stack")
}


#' @export
#' @rdname fifo
#' 
is.fifo <- 
function(x)
{
  is.stack(x) && inherits(x, "fifo")
}


#' @export
#' @rdname fifo
#' 
is.lifo <- 
function(x)
{
  is.stack(x) && inherits(x, "lifo")
}


#' @export
#' @rdname fifo
#' 
is.nino <- 
function(x)
{
  is.stack(x) && inherits(x, "nino")
}


#' @export
#' @rdname fifo
#'
as.list.stack <-
function(x,
         ...)
{
  unclass(x)
}
