#' @title 
#' Stacks - creation and class
#' 
#' @description 
#' The \code{fifo}, \code{lifo}, and \code{nino} functions 
#' create 'First In First Out',  'Last In First Out', and 
#' 'Not In or Never Out' stacks, respectively. 
#' 
#' @param max_length
#' numeric. The maximum (infinite by default) number of objects 
#' the stack can contain. 
#' 
#' @param max_size
#' numeric. The maximum (infinite by default) size of  
#' the stack, in octets.  
#' 
#' @param x
#' An object to be tested or coerced. 
#' 
#' @param ...
#' Additional arguments. 
#' 
#' @return 
#' \code{is.xxx} functions return a logical. 
#' 
#' \code{fifo}, \code{lifo}, and \code{nino} return 
#' an empty FIFO, LIFO, or NINO stack. 
#' 
#' @seealso 
#' \code{\link[flifo]{push}}, 
#' \code{\link[flifo]{pop}}. 
#' 
#' @export
#' 
fifo <- 
function(max_length = Inf, 
         max_size = Inf)
{
  x <- list()
  class(x) <- c("fifo", "stack")
  attr(x, "sizes") <- 0
  attr(x, "max_length") <- max_length
  attr(x, "max_size") <- max_size
  x
}

#' @export
#' @rdname fifo
#' 
lifo <- 
function(max_length = Inf, 
         max_size = Inf)
{
  x <- list()
  class(x) <- c("lifo", "stack")
  attr(x, "sizes") <- 0
  attr(x, "max_length") <- max_length
  attr(x, "max_size") <- max_size
  x
}


#' @export
#' @rdname fifo
#' 
nino <- 
function(max_length = Inf, 
         max_size = Inf)
{
  x <- list()
  class(x) <- c("nino", "stack")
  attr(x, "sizes") <- 0
  attr(x, "max_length") <- max_length
  attr(x, "max_size") <- max_size
  x
}
