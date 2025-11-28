#' @title fisher.sim
#' @description Performs Monte carlo's simulation for Fisher's exact test on GPU.
#'
#' @param x a vclMatrix of integers.
#' @param N an integer specifying number of replicates.
#' @param streams a vclMatrix of streams. Default using streams with package default initial seeds.
#' @param Nglobal a (non-empty) integer vector specifying size of the index space on GPU for use, with default value from global option 'clrng.Nglobal'.
#' @param type a character string specifying "double" or "float" of the returned test statistics, with default value from global option 'clrng.type'.
#' @param returnStatistics a logical value, if TRUE, return test statistics, default is FALSE.
#' @param verbose a logical value, if TRUE, print extra information, default is FALSE.
#' @import gpuR
#' 
#' @return a `htest' object of p-value and actual number of replicates and a list of test statistics, streams used, threshold.
#' 
#'
#' @examples 
#' library('clrng')
#' if (detectGPUs() >= 1) {
#'   setContext(grep("gpu", listContexts()$device_type)[1])
#'   
#'   Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4)
#'   Job <- gpuR::vclMatrix(Job, type="integer")
#'   
#'   getOption('clrng.type')
#'   options(clrng.Nglobal = c(64,16))
#'   streams <- createStreamsGpu()
#'   result <- fisher.sim(Job, 1e5, streams = streams)
#'   print(result)
#'   result$streams
#'   result$threshold
#' } else {
#'   message("No GPU context available")
#' }
#' 
#' @useDynLib clrng
#' @export



fisher.sim=function(
    x, # a vclMatrix
    N, # requested number of simualtions,
    streams,
    Nglobal = getOption('clrng.Nglobal'),
    type =  getOption('clrng.type'),
    returnStatistics = FALSE,
    verbose = FALSE){
  
  if (is.null(Nglobal)) stop("Nglobal is missing")
  if (is.null(type))   stop('precision type missing')

  
  
  # lfactorial(x)=lgamma(x+1)
  # STATISTIC <- -sum(lfactorial(x))          ##STATISTIC is negative
  # almost.1 <- 1 + 64 * .Machine$double.eps
  # 
  # threshold = STATISTIC/almost.1
  
  if(missing(streams)) {
    stop("streams must be supplied")
  }
  
  if(!isS4(streams)) {
    warning("streams should be a S4 matrix")}
  
  if(prod(Nglobal) != nrow(streams))
    warning("number of work items needs to be same as number of streams")
  
  Nlocal = c(1, 1)
  
  if(verbose) {
    cat('local sizes ', toString(Nlocal), '\nglobal sizes ', toString(Nglobal),
        '\n streams ', toString(dim(streams)), '\n')
  }
  
  
  # sr0 <- rowSums(x)
  # sc0 <- colSums(x)
  # 
  #   
  # ## we drop all-zero rows and columns
  # x <- x[sr0 > 0, sc0 > 0, drop = FALSE]
  # 
  # xVcl<-gpuR::vclMatrix(x, type='integer') 
  
  #  print(class(xVcl))
  
  simPerItem<-ceiling(N/prod(Nglobal))
  TotalSim<-simPerItem*prod(Nglobal)
  
  if(returnStatistics) {
    results <- gpuR::vclVector(length=as.integer(TotalSim), type=type)
  } else {
    results <- gpuR::vclVector(length=as.integer(1), type=type)
  }
  
  
  PVAL <- NULL
  
  counts<- cpp_gpuFisher_test(x, results, simPerItem, streams, Nglobal,Nlocal)
  
  #theTime<-system.time(cpp_gpuFisher_test(x, results, as.integer(B), streams, Nglobal,Nlocal))
  
  
  # if(verbose)
  #print(theTime)
  #time 
  
  PVAL <- (1 + as.integer(counts[2]) ) / (TotalSim + 1)
  #counts<-10
  #PVAL<-0.1
  # format(PVAL, digits=5)
  #if(class(PVAL) == 'try-error') {
  #  PVAL = counts
  #}
  
  if (returnStatistics){
    
    theResult <- list(
      data.name = deparse(substitute(x)),
      p.value = PVAL,
      method = paste("Fisher's Exact Test for Count Data", "with simulated p-value\n\t (based on", TotalSim,
                     "replicates)"),
      threshold = counts[1],
      streams=streams,
      simNum = TotalSim,
      counts=counts[2], 
      sim = results
    )
    
  }else{
    
    # Construct htest object
    theResult <- list(
      data.name = deparse(substitute(x)),
      p.value = PVAL,
      method = paste("Fisher's Exact Test for Count Data", "with simulated p-value\n\t (based on", TotalSim,
                     "replicates)"),
      threshold = counts[1],
      streams=streams,
      simNum = TotalSim,
      counts=counts[2])
  }
  
  attr(theResult, "class") <- "htest"
  theResult
  
}
































