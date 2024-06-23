#' @title runifGpu
#' @description Generate uniform random numbers parallely on a GPU.
#' 
#' @param n a number or a numeric vector specifying the size of output vector or matrix.
#' @param streams a vclMatrix of streams. 
#' @param Nglobal a (non-empty) integer vector specifying size of work items for use, with default value from global option 'clrng.Nglobal'.
#' @param type a character string specifying "double" or "float" of random numbers, with default value from global option 'clrng.type'.
#' @param verbose a logical value, if TRUE, print extra information, default is FALSE.
#' @import gpuR
#' @importFrom utils capture.output
#' 
#' @details \code{type} specifies the precision type of random numbers. If GPU supports "double", 'clrng.Nglobal' is "double", otherwise, `clrng.Nglobal' is "single".
#' 
#' @return a 'vclVector' or 'vclMatrix' of uniform random numbers.
#' @examples  
#' library('clrng')
#' if (detectGPUs() >= 1) {
#'   setContext(grep("gpu", listContexts()$device_type)[1])
#'   currentDevice()
#'   getOption('clrng.Nglobal')
#'   streams <- createStreamsGpu()
#'   as.vector(runifGpu(5, streams))
#' 
#'   # Change global options 
#'   options(clrng.type="float")
#'   # produce a matrix of random numbers
#'   as.matrix(runifGpu(c(2,2), streams))} else {
#'   message("No GPU context available")
#' }
#' 
#' @useDynLib clrng
#' @export


runifGpu = function(
    n, 
    streams, 
    Nglobal = getOption('clrng.Nglobal'),
    type = getOption('clrng.type'),
    verbose = FALSE) {
  
  
  if (is.null(Nglobal)) stop("Nglobal is missing")
  if (is.null(type))   stop('precision type missing')
  
  if(length(n)>=3){
    stop("'n' has to be a vector of no more than two elements")
  }
  if(length(n)==0){
    stop("specify the number of rows and columns of the output matrix")
  }
  if(length(n)==1){
    n<-c(n,1)
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
  
  xVcl<-gpuR::vclMatrix(0L, nrow=n[1], ncol=n[2], type=type[1])    
  
  
  gpuRnBackend(xVcl, streams, Nglobal,"uniform", verbose) 
  
  invisible(streams)
  
  if(ncol(xVcl)==1) {
    if (type == "float"){
      invisible(capture.output(xVcl <- as.vclVector(xVcl)))     # an unneeded message from gpuR if float
    }else{
      xVcl <- as.vclVector(xVcl)
    }
    
  }
  
  xVcl
  
}




























  