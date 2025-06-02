#' @title 
#' Insert an object into a stack 
#' 
#' @description 
#' The \code{push} function inserts an 
#' object into \code{.stack}. 
#' 
#' @details 
#' The \code{push} function is not pure. Side effects (made on purpose) are: 
#' \itemize{
#'   \item \code{.stack} is modified in the calling environment; 
#'   \item \code{x} is removed (deleted) if it exists in the calling environment. 
#' }
#' 
#' @param .stack
#' A stack. 
#' 
#' @param x
#' An object to insert in \code{.stack}. 
#' 
#' @return 
#' \code{NULL} is returned invisibly. 
#' 
#' @seealso 
#' \code{\link[flifo]{pop}}. 
#' 
#' @importFrom pryr object_size
#' @export
#' 
#' @examples 
#' (s <- lifo(max_length = 3)) # empty LIFO
#' (push(s, 0.3)) #
#' (push(s, data.frame(x=1:2, y=2:3))) 
#' obj <- pop(s) # get the last element inserted
#' 
push <- 
function(.stack, 
         x)
{
  s <- deparse(substitute(.stack))
  env <- parent.frame()
  if (!exists(s, envir = env)) {
    stop("'.stack' does not exist in the calling environment")
  }

  if (!is.stack(.stack)) stop("'.stack' must be a stack")
  if (missing(x)) stop("'x' is missing")
  nx <- deparse(substitute(x))

  cl <- class(.stack)
  size_of_x <- as.numeric(pryr::object_size(x))
  ml <- max_length(.stack)
  si <- sizes(.stack)
  ms <- max_size(.stack)
  if (length(.stack)+1L > ml || sum(si)+size_of_x > ms) stop("'.stack' is full")
  
  ## Update '.stack'
  if (is.fifo(.stack)) {
    .stack <- c(.stack, list(x))
    sizes(.stack) <- c(si, size_of_x)
  } else if (is.lifo(.stack)) {
    .stack <- c(list(x), .stack)
    sizes(.stack) <- c(size_of_x, si)
  } else if (is.nino(.stack)) {
    .stack <- c(list(c()), .stack)
    sizes(.stack) <- c(size_of_x, si)
  }
  class(.stack) <- cl
  max_length(.stack) <- ml
  max_size(.stack) <- ms
  assign(s, .stack, envir = env, inherits = TRUE)

  ## Remove 'x' from the calling environment
  if (exists(nx, envir = env, inherits = TRUE)) {
    rm(list = nx, envir = env, inherits = TRUE)
  }
  
  invisible(NULL)
}
