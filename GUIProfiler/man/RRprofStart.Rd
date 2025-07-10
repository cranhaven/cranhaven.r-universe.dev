\name{RRprofStart}
\alias{RRprofStart}
\title{
RRprofStart
}
\description{
Rprof() is activated and started
}
\usage{
RRprofStart(filename = "RRprof.out", interval = 0.02, numfiles = 100L, bufsize = 10000L)
}
\arguments{
  \item{filename}{
The file to be used for recording the profiling results.
}
  \item{interval}{
real: time interval between samples. 
}
  \item{numfiles}{
integers: line profiling memory 
}
  \item{bufsize}{
allocation
}
}
\author{
Fernando de Villar and Angel Rubio
}
\note{
The profiler interrupts R asynchronously, and it cannot allocate memory to store results as it runs. This affects line profiling, which needs to store an unknown number of file pathnames. The numfiles and bufsize arguments control the size of pre-allocated buffers to hold these results: the former counts the maximum number of paths, the latter counts the numbers of bytes in them. If the profiler runs out of space it will skip recording the line information for new files, and issue a warning when Rprof(NULL) is called to finish profiling.

The timing interval cannot be too small, for the time spent in each profiling step is added to the interval. What is feasible is machine-dependent, but 10ms seemed as small as advisable on a 1GHz machine.
}


\seealso{
\code{\link[GUIProfiler]{RRprofStop}}, \code{\link[GUIProfiler]{RRprofReport}}, \code{\link{Rprof}}
}
\examples{
temp<-tempdir()
# Definition of two functions
normal.solve <- function(A,b) {
  Output <- solve(crossprod(A), t(A)\%*\%b)
}

chol.solve <- function(A,b) {
  L <- chol(crossprod(A))
  Output1 <- backsolve(L, t(A)\%*\%b, transpose=TRUE)
  Output2 <- backsolve(L, Output1)
}

compareMethods <- function() {
  library(MASS)
  # Call the functions
  source(paste(temp,"/normal.solve.R",sep=""))
  source(paste(temp,"/chol.solve.R",sep=""))
  # Solving a big system of equations
  nrows <- 1000
  ncols <- 500
  A <- matrix(rnorm(nrows*ncols),nrows,ncols)
  b <- rnorm(nrows)
  # Testing different possibilities
  Sol1 <- qr.solve(A,b) # Using QR factorization
  Sol2 <- coefficients(lm.fit(A,b)) # lm.fit, based on QR but with some overhead
  Sol3 <- ginv(A) \%*\% b # Using the pseudoinverse based on SVD
  Sol4 <- normal.solve(A,b) # Using a function based on the normal equations.
  Sol5 <- chol.solve(A,b) # Using a function based on the Choleski factorization.
}

# Dump these functions to three different files

dump("normal.solve",file=paste(temp,"/normal.solve.R",sep=""))
dump("chol.solve",file=paste(temp,"/chol.solve.R",sep=""))
dump("compareMethods",file=paste(temp,"/compareMethods.R",sep=""))
source(paste(temp,"/compareMethods.R",sep=""))

# Profile the code

RRprofStart()
compareMethods()
RRprofStop()
# Uncomment to open the report
#RRprofReport()
}