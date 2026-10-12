################################################################################
# Copyright 2016 Indiana University
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
################################################################################

#' Performs microbenchmarking of a dense matrix linear algebra kernel
#'
#' \code{MicrobenchmarkDenseMatrixKernel} performs microbenchmarking of a
#' dense matrix linear algebra kernel for several matrix dimensions
#'
#' This function performs microbenchmarking of a dense matrix linear algebra
#' kernel for several matrix dimensions and a given number of threads.  The
#' kernel to be performance tested, the matrix dimensions to be tested, and
#' other parameters specifying how the kernel is to be benchmarked are given in
#' the input object \code{benchmarkParameters} which is an instance of
#' the class \code{\link{DenseMatrixMicrobenchmark}}.  For each matrix dimension
#' to be tested, the run time performance of the kernel is averaged over
#' multiple performance trials, and the averages are written to a CSV file.  The
#' results of the individual performance trials are retained in a data frame
#' that is returned upon completion of the microbenchmark.  The kernel can also
#' be executed with multiple threads if the kernel supports multithreading.  See
#' \code{\link{DenseMatrixMicrobenchmark}} for more details on the benchmarking
#' parameters.
#' 
#' @param benchmarkParameters an object of type
#'   \code{\link{DenseMatrixMicrobenchmark}} specifying the matrix
#'   dimensions of matrices to be tested and the number of performance trials
#'   to perform for each matrix dimension.
#' @param numberOfThreads the number of threads the microbenchmark is being
#'   performed with.  The value is for informational purposes only and does not
#'   effect the number threads the kernel is executed with.
#' @param resultsDirectory a character string specifying the directory
#'   where all of the CSV performance results files will be saved
#' @param runIdentifier a character string specifying the suffix to be
#'   appended to the base of the file name of the output CSV format files
#' @return a dataframe containing the performance trial times for each matrix
#'   tested, that is the raw performance data before averaging.  The columns
#'   of the data frame are the following:
#'   \describe{
#'     \item{BenchmarkName}{The name of the microbenchmark}
#'     \item{DimensionParameter}{The dimension parameters the microbenchmark
#'        uses to define the matrix dimensions to be tested with}
#'     \item{UserTime}{The amount of time spent in user-mode code within the
#'        microbenchmarked code}
#'     \item{SystemTime}{The amount of time spent in the kernel within the
#'        process}
#'     \item{WallClockTime}{The total time spent to complete the performance
#'        trial}
#'     \item{DateStarted}{The date and time the performance trial was commenced}
#'     \item{DateFinished}{The date and time the performance trial ended}
#'   }
#'
MicrobenchmarkDenseMatrixKernel <- function(benchmarkParameters,
   numberOfThreads, resultsDirectory, runIdentifier) {

   resultsFrame <- data.frame(BenchmarkName=character(),
      DimensionParameter=integer(), UserTime=numeric(),
      SystemTime=numeric(), WallClockTime=numeric(), DateStarted=character(),
      DateFinished=character(), stringsAsFactors=FALSE)
   
   # Make sure needed parameters exist
   parameterCheck <- tryCatch({
      cat(sprintf("Running microbenchmark: %s\n", benchmarkParameters$benchmarkName))
      cat(sprintf("Microbenchmark description: %s\n", benchmarkParameters$benchmarkDescription))
      allocator <- match.fun(benchmarkParameters$allocatorFunction)
      benchmark <- match.fun(benchmarkParameters$benchmarkFunction)

      dimensionParameters <- benchmarkParameters$dimensionParameters
      numberOfDimensions <- length(dimensionParameters)
      numberOfTrials <- benchmarkParameters$numberOfTrials
      numberOfWarmupTrials <- benchmarkParameters$numberOfWarmupTrials
      benchmarkName <- benchmarkParameters$benchmarkName
      csvResultsBaseFileName <- benchmarkParameters$benchmarkName
      csvResultsFileName <- file.path(resultsDirectory,
         paste(csvResultsBaseFileName, "_", runIdentifier, ".csv", sep=""))

      if (numberOfDimensions < 1) {
         stop(sprintf("ERROR: Input checking failed for microbenchmark '%s'  -- length of dimensionParameters array must be greater than zero", benchmarkName))
      }

      if (numberOfDimensions != length(numberOfTrials)) {
         stop(sprintf("ERROR: Input checking failed for microbenchmark '%s'  -- length of numberOfTrials and dimensionParameters arrays must be equal", benchmarkName))
      }

      if (numberOfDimensions != length(numberOfWarmupTrials)) {
         stop(sprintf("ERROR: Input checking failed for microbenchmark '%s' -- length of numberOfWarmupTrials and dimensionParameters arrays must be equal", benchmarkName))
      }

      dir.create(resultsDirectory, showWarnings=FALSE, recursive=TRUE)

      TRUE
   }, warning = function(war) {
      write(sprintf("%s", war), stderr())
      return(FALSE)
   }, error = function(err) {
      write(sprintf("%s", err), stderr())
      return(FALSE)
   })

   # Return an empty results frame if there were problems with the
   # input parameters
   if (!parameterCheck) {
      return(resultsFrame)
   }


   maximumNumberOfTrials <- max(numberOfTrials)
   trialTimes <- rep(NA_real_, maximumNumberOfTrials*numberOfDimensions)
   dim(trialTimes) <- c(maximumNumberOfTrials, numberOfDimensions)
   averageWallClockTimes <- rep(NA_real_, numberOfDimensions)
   standardDeviations <- rep(NA_real_, numberOfDimensions)
   numberOfSuccessfulTrials <- rep(0, numberOfDimensions)

   # Run the microbenchmark for different size matrices whose dimensions are
   # given in a vector of dimensions
   for (j in 1:numberOfDimensions) {
      d <- dimensionParameters[j]

      for (i in 1:(numberOfTrials[j]+numberOfWarmupTrials[j])) {
         cat(sprintf("Running performance trial %d for matrix dimension %d x %d...\n", i, d, d))

         allocationSuccessful <- tryCatch({
            RNGkind(kind=RBenchmarkOptions$rng.kind,
               normal.kind=RBenchmarkOptions$rng.normal.kind)
            set.seed(RBenchmarkOptions$rng.seed,
               kind=RBenchmarkOptions$rng.kind,
               normal.kind=RBenchmarkOptions$rng.normal.kind)
            kernelParameters <- allocator(benchmarkParameters, j)
            TRUE
         }, warning = function(war) {
            msg <- sprintf("ERROR: allocator threw a warning -- %s", war)
            write(msg, stderr())
            return(FALSE)
         }, error = function(err) {
            msg <- sprintf("ERROR: allocator threw an error -- %s", err)
            write(msg, stderr())
            return(FALSE)
         })

         if (!allocationSuccessful) {
            break;
         }

         dateStarted <- date()
         timings <- c(NA_real_, NA_real_, NA_real_)

         benchmarkSuccessful <- tryCatch({
            timings <- benchmark(benchmarkParameters, kernelParameters)
            TRUE
         }, warning = function(war) {
            msg <- sprintf("WARN: benchmark threw a warning -- %s", war)
            write(msg, stderr())
            return(FALSE)
         }, error = function(err) {
            benchmarkSuccessful <- FALSE
            msg <- sprintf("ERROR: benchmark threw an error -- %s", err)
            write(msg, stderr())
            return(FALSE)
         })
            
         if (!benchmarkSuccessful) {
            break;
         }

         dateFinished <- date()
         userTime <- timings[1]
         systemTime <- timings[2]
         wallClockTime <- timings[3]

         if (i > numberOfWarmupTrials[j]) {
            numberOfSuccessfulTrials[j] <- numberOfSuccessfulTrials[j] + 1 
            resultsFrame[nrow(resultsFrame)+1, ] <- list(
               benchmarkName, as.integer(dimensionParameters[j]),
               userTime, systemTime, wallClockTime,
               dateStarted, dateFinished)
         }
 
         remove(kernelParameters)
         invisible(gc())

         if (i > numberOfWarmupTrials[j]) {
            trialTimes[i-numberOfWarmupTrials[j], j] <- wallClockTime
         }

         cat(sprintf("done: %f(sec)\n", wallClockTime))
      }

      averageWallClockTimes[j] <- ComputeAverageTime(
         numberOfSuccessfulTrials[j], trialTimes[,j]) 
      standardDeviations[j] <- ComputeStandardDeviation(
         numberOfSuccessfulTrials[j], trialTimes[,j])
      WriteDenseMatrixPerformanceResultsCsv(numberOfThreads,
         dimensionParameters[j], averageWallClockTimes[j],
         standardDeviations[j], csvResultsFileName)
   }

   PrintDenseMatrixMicrobenchmarkResults(benchmarkName, numberOfThreads,
      dimensionParameters, numberOfSuccessfulTrials, trialTimes,
      averageWallClockTimes, standardDeviations)

   return(resultsFrame)
}
