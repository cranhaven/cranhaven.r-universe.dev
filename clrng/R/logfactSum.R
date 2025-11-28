#' @title logfactSum
#' @description Computes the log-factorial of a table on a GPU.
#' 
#' @param x a matrix of integers.
#' @param Nglobal a (non-empty) integer vector specifying size of the index space for use.
#' @return sum of log-factorials of elements of the input matrix `x'.
#' 
#' @examples 
#' 
#' library(clrng)
#' if (detectGPUs() >= 1) {
#' setContext(grep("gpu", listContexts()$device_type)[1])
#' 
#' x <- matrix(c(1:36), 6,6)
#' logfactSum(x, c(2,2))
#' # note if matrix is not of integers, a warning will be displayed, eg.
#' x2 <- matrix(c(1.1,2.1,3.1,4.1,5.1,6.1,7.1,8.1,9.1), 3,3)
#' is.integer(x2)
#' logfactSum(x2, c(16,16))} else {
#'   message("No GPU context available")
#' }
#' 
#' @useDynLib clrng
#' @export




logfactSum <- function(x,      # an R matrix
                       Nglobal = getOption('clrng.Nglobal')) {
  
  
  if(any(dim(x) < 2L))
    stop("table must have at least 2 rows and columns")
  
  if (is.null(Nglobal)) stop("Nglobal is missing")
  
  if(!is.integer(x)) {
    xo <- x
    x <- round(x)
    if(any(x > .Machine$integer.max))
      stop("'x' has entries too large to be integer")
    if(!identical(TRUE, (ax <- all.equal(xo, x))))
      warning(gettext("matrix has been rounded to integer", ax), domain = NA)
    storage.mode(x) <- "integer"
  }
  
  
  x <- gpuR::vclMatrix(x,type="integer")
  
  
  result <- logfactsumBackend(x, Nglobal)
  
  
  result
  
  
  
}







