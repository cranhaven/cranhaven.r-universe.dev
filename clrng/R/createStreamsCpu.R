#' @title createStreamsCpu
#' @description Create streams stored on the CPU.
#' @param n a integer specifying number of streams to create, default is the number of total work items in use.
#' @return an R matrix of streams on CPU.
#' 
#' @examples 
#' library(clrng)
#' if (detectGPUs() >= 1) {
#'   t(createStreamsCpu(n=5))
#'   ## GPU streams
#'   myStreamsGpu = vclMatrix(createStreamsCpu(n=4)) }else {
#'   message("No GPU context available")
#' }
#' 
#' @useDynLib clrng



#' @export

createStreamsCpu = function(n=prod(getOption('clrng.Nglobal'))){
  
  n = as.integer(n)
  
  if(!exists(".Random.seed.clrng", envir = .GlobalEnv)) {
    setBaseCreator()
  } 
  
  
  streamsR<-gpuR::vclMatrix(0L, nrow=as.integer(n), ncol=12, type="integer")
  streamsCpu<- matrix(0L, nrow=as.integer(n), ncol=12)
  
  currentCreator = CreateStreamsBackend(
    .Random.seed.clrng,  
    streamsR,
    streamsCpu,
    onGpu=FALSE,
    keepInitial=TRUE)
  # 
  assign(".Random.seed.clrng", currentCreator, envir = .GlobalEnv)
  streamsCpu
  
}    

utils::globalVariables(".Random.seed.clrng")



   

