#' @title setBaseCreator
#' @description Set the initial seed of the first stream.
#' @param initial a vector of six 31-bit integers specifying the initial state of the first stream. Default is c(12345,12345,12345,12345,12345,12345).     
#' @return a stream object of class 'vclMatrix'.
#' 
#' @details \code{initial} is of length 6, recycled if shorter.
#' 
#' @examples
#' library('clrng')
#' setBaseCreator(c(111,222,333,444,555,666))
#' @useDynLib clrng



#' @export
setBaseCreator <- function(initial = rep(12345,6)) {
  
  initial = as.integer(initial)
  
  if(any(initial[1:3] >= rep(2147483647,3))){     # mrg31k3p_M1
    stop('CLRNG_INVALID_SEED')
  }
  
  if(any(initial[4:6] >= rep(2147462579,3))){    # mrg31k3p_M2
    stop('CLRNG_INVALID_SEED')
  }
  
  if(length(initial) != 6){
    # message('initial seed should be a vector of 6 integers!')
    initial = rep_len(initial, 6)
  }
  # Check that the seeds have valid values
  if(any(initial < 0))
    stop('CLRNG_INVALID_SEED')
  
  
  if(all(initial[1:3] == c(0,0,0)) | all(initial[4:6] == c(0,0,0)))
    stop('CLRNG_INVALID_SEED')
  pos <- 1
  assign(".Random.seed.clrng", initial, envir = as.environment(pos))
}









#' @title createStreamsGpu
#' @description Generate streams on a GPU device
#' @param n a integer specifying number of streams to create, default is the number of total work items in use
#' @return a stream object of class 'vclMatrix' on GPU
#' @examples
#' library(clrng)
#' if (detectGPUs() >= 1) {
#'  setBaseCreator(rep(12345,6))
#'  setContext(grep("gpu", listContexts()$device_type)[1])
#'  myStreamsGpu = createStreamsGpu(4)
#'  t(as.matrix(myStreamsGpu))
#' 
#'  myStreamsGpu2 = createStreamsGpu(6)
#'  t(as.matrix(myStreamsGpu2)) }else {
#'   message("No GPU context available")
#' }
#' 
#' @useDynLib clrng    
#' @export
createStreamsGpu = function(n=prod(getOption('clrng.Nglobal'))){
  
  streamsGpu<-gpuR::vclMatrix(0L, nrow=as.integer(n), ncol=12, type="integer")
  streamsCpu<- matrix(0L, nrow=as.integer(n), ncol=12)
  pos <- 1
  if(!exists(".Random.seed.clrng", envir = as.environment(pos))) {
    setBaseCreator()
  } 
  
  
  currentCreator = CreateStreamsBackend(
    get(".Random.seed.clrng", envir = as.environment(pos)),   
    streamsGpu,
    streamsCpu,
    onGpu=TRUE,
    keepInitial=TRUE)
  
  # gpuR::colnames(streamsR) = c("current.g1.1", "current.g1.2", "current.g1.3", "current.g2.1", "current.g2.2", "current.g2.3",
  #                              "initial.g1.1", "initial.g1.2", "initial.g1.3", "initial.g2.1", "initial.g2.2", "initial.g2.3"
  #                              # "substream.g1.1", "substream.g1.2", "substream.g1.3", "substream.g2.1", "substream.g2.2", "substream.g2.3"
  # )
  
  assign(".Random.seed.clrng", currentCreator, envir = as.environment(pos))
  streamsGpu
  
}

# utils::globalVariables(".Random.seed.clrng")


