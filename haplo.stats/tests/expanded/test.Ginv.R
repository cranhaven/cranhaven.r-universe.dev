

## package: haplo.stats
## test script: Ginv

## settings

verbose=TRUE

require(haplo.stats)

# test Ginv on a matrix that is knowns to cause problems for LINPACK
# implementation of svd used in Ginv.

# this varx matrix comes from running haplo.scan (within haplo.stats)
# on the 11-loci hla.demo dataset.  
source("dump.varx.s")

# Jason Sinnwell 3/2004

  if(verbose) cat("setting up data...\n")

  if(verbose) cat("test matrix (441x441) that causes error in LINPACK, but not LAPACK svd\n")

  ginv.varx <- Ginv(varx)
  
# Ginv.q version 1.4 is known to fail and get a numerical error

# ginv.varx$Ginv[1:5,1:5]

  ginv.varx.eps <- Ginv(varx, eps=1e-4)

#  ginv.varx.eps$Ginv[1:5,1:5]

if(verbose) cat("the zero matrix should give ginv=0, rank=0\n")
  zeroMat <- matrix(0)
  epsMat <- matrix(1e-7)
  zeroGinv <- Ginv(zeroMat)
  epsGinv <- Ginv(epsMat)
  
  # a matrix with an NA should give a warning
#  naMat <- matrix(c(1,0,NA,0,2,1,1,1,3),ncol=3)
#  na.ginv <- Ginv(naMat)


# if(verbose) cat("sinking print results to a file\n")
  
#if(update) {
#  sink(file=goldfile)
#}  else {
#  sink("sink.Ginv.out")
#}

 ## print all testable results (via a diff command) to a file  
print(ginv.varx$Ginv[1:5,1:5])

ginv.varx$rank

print(ginv.varx.eps$Ginv[1:5,1:5])

ginv.varx.eps$rank

print(zeroGinv)

print(epsGinv)

#sink()
  



