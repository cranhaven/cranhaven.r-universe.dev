## ----packages, results='hide', message=FALSE----------------------------------
library("clrng")

## ----teaTasting, eval=T-------------------------------------------------------
TeaTasting <-matrix(c(3, 1, 1, 3),nrow=2)
TeaTasting

## using R's fisher.test()
fisher.test(TeaTasting)$p.value

## ----teaTasting on GPU, eval=T------------------------------------------------
if (detectGPUs()) {
  setContext(grep("gpu", listContexts()$device_type)[1])
  
  ## get current device name
  gpuInfo()$deviceName
  ## using clrng's fisher.sim()
  ## check the size of work items and GPU precision type at the moment
  getOption('clrng.Nglobal')
  getOption('clrng.type')
  
  ## convert the data table to be on GPU
  TeaTastingGpu<-gpuR::vclMatrix(TeaTasting,type="integer")
  
  ## choose the size of Nglobal and create 16*64 streams
  options(clrng.Nglobal=c(16,64))
  streams <- gpuR::vclMatrix(clrng::createStreamsCpu())
  
  ## perform 100000 fisher's simulation on GPU and return all test statistics
  result<-clrng::fisher.sim(TeaTastingGpu, N=1e5, streams=streams,returnStatistics=TRUE)
  print(result)
  
  ## show some simulation results
  result$threshold
  as.vector(result$sim)[10:20]
  length(result$sim)
  
} else {
  message("No GPU detected. Skipping GPU-dependent code.")
}


