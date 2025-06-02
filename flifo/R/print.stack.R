#' @title 
#' Print a stack. 
#' 
#' @description 
#' The function \code{print.stack} 
#' prints the class of the stack \code{x} (FIFO, LIFO, or NINO) and displays 
#' its next reachable object. 
#' 
#' @param x
#' A stack. 
#' 
#' @param ... 
#' Additional arguments. 
#' 
#' @return 
#' The stack \code{x} is returned invisibly. 
#' 
#' @seealso 
#' \code{\link[flifo]{push}}, \code{\link[flifo]{pop}}. 
#' 
#' @export
#' 
print.stack <-
function(x,
         ...)
{
  tx <- toupper(class(x)[[1L]])
  if (is.empty(x)) {
    cat(tx, "stack is empty\n")
    
  } else if (is.nino(x)) {
    cat(tx, "stack: no element can be reached\n")
    
  } else {
    cat(tx, "stack: next reachable element is:\n")
    cat("\n")
    print(as.list(x)[[1L]], ...)
  }
  invisible(x)
}
