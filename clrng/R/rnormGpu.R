#' @title rnormGpu
#' @description Generate standard Normal random numbers parallely on a GPU.
#' 
#' @param n a number or a vector specifying the size of output vector or matrix.
#' @param streams a vclMatrix of streams. 
#' @param Nglobal a (non-empty) integer vector specifying size of work items for use, with default value from global option 'clrng.Nglobal'.
#' @param type a character string specifying "double" or "float" of random numbers, with default value from global option 'clrng.type'.
#' @param verbose a logical value, if TRUE, print extra information, default is FALSE.
#' @import gpuR
#' @return a 'vclVector' or 'vclMatrix' of standard Normal random numbers.
#' 
#' @details \code{type} specifies the precision type of random numbers. If GPU supports "double", 'clrng.Nglobal' is "double", otherwise, `clrng.Nglobal' is "single".
#' 
#' @examples 
#' library(clrng)
#' if (detectGPUs() >= 1) {
#'   setContext(grep("gpu", listContexts()$device_type)[1])
#'   currentPlatform()
#'   streams <- createStreamsGpu()
#'   as.vector(rnormGpu(7, streams=streams))
#'
#'   getOption('clrng.Nglobal')
#'   # use float precision and global size
#’   options(clrng.type="float", clrng.Nglobal = c(4,2)) 
#'   as.matrix(rnormGpu(c(2,3), streams=streams))} else {
#'   message("No GPU context available")
#' }
#' 
#' @useDynLib clrng
#' @export



rnormGpu = function(
    n, 
    streams, 
    Nglobal = getOption('clrng.Nglobal'),
    type = getOption('clrng.type'),
    verbose = FALSE) {
  
  if (is.null(Nglobal)) stop("Nglobal is missing")
  if (is.null(type))   stop('precision type missing')
  
  if(any(grepl("vclMatrix", class(n)))) {
    xVcl = n
  } else {
    if(length(n)>=3){
      stop("'n' has to be a vector of no more than two elements")
    }
    if(length(n)==0){
      stop("specify the number of rows and columns of the output matrix")
    }
    if(length(n)==1){
      n<-c(1,n)
    }
    xVcl<-gpuR::vclMatrix(0, nrow=n[1], ncol=n[2], type=type[1])
  }
  
  if(Nglobal[2]%%2 !=0 ){
    stop("number of work items in dimension 2 must be a multiple of 2")
  }
  
  if(missing(streams)) {
    stop("streams must be supplied")
  }

  if(prod(Nglobal) > nrow(streams)){
    stop("the number of streams for use should always equal (or exceed)
          the maximum number of work items to be used")
  }
  
  
  gpuRnBackend(xVcl, streams, Nglobal,"normal", verbose) 
  
  #  invisible(streams)
  
  if( nrow(xVcl)==1 & is.numeric(n)) xVcl = xVcl[1,]
  
  xVcl
  
}






