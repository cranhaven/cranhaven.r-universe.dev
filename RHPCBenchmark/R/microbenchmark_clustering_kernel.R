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

#' Performs microbenchmarking of a clustering for machine learning kernel
#'
#' \code{MicrobenchmarkClusteringKernel} performs microbenchmarking of a
#' clustering for machine learning kernel for a given data set
#'
#' This function performs microbenchmarking of a clustering for machine learning
#' kernel for a given data set and a given number of threads.  The
#' kernel to be performance tested and other parameters specifying how the
#' kernel is to be benchmarked are given in the input object
#' \code{benchmarkParameters} which is an instance of the class
#' \code{\link{ClusteringMicrobenchmark}}.  The
#' performance results are averaged over the number of performance trials
#' and written to a CSV file.  The results of the individual performance
#' trials are retained in a data frame that is returned upon completion of the
#' microbenchmark.  The kernel can be executed with multiple threads if the
#' kernel supports multithreading.  See
#' \code{\link{ClusteringMicrobenchmark}} for more details on the
#' benchmarking parameters.
#' 
#' @param benchmarkParameters an object of type
#'   \code{\link{ClusteringMicrobenchmark}} specifying the data set
#'   to be read in or generated and the number of performance trials
#'   to perform with the data set.
#' @param numberOfThreads the number of threads the microbenchmark is being
#'   performed with.  The value is for informational purposes only and does not
#'   effect the number threads the kernel is executed with.
#' @param resultsDirectory a character string specifying the directory
#'   where all of the CSV performance results files will be saved
#' @param runIdentifier a character string specifying the suffix to be
#'   appended to the base of the file name of the output CSV format files
#' @return a dataframe containing the performance trial times for the given
#'   kernel and data set being tested, that is the raw performance data before
#'   averaging.  The columns of the data frame are the following:
#'   \describe{
#'     \item{BenchmarkName}{The name of the microbenchmark}
#'     \item{NumberOfFeatures}{The number of features in each feature vector}
#'     \item{NumberOfFeatureVectors}{The number of features in the data set}
#'     \item{NumberOfClusters}{The number of clusters in the data set}
#'     \item{UserTime}{The amount of time spent in user-mode code within the
#'       microbenchmarked code}
#'     \item{SystemTime}{The amount of time spent in the kernel within the
#'       process}
#'     \item{WallClockTime}{The total time spent to complete the performance
#'       trial}
#'     \item{DateStarted}{The date and time the performance trial was commenced}
#'     \item{DateFinished}{The date and time the performance trial ended}
#'   }
#'
MicrobenchmarkClusteringKernel <- function(benchmarkParameters,
   numberOfThreads, resultsDirectory, runIdentifier) {

   resultsFrame <- data.frame(BenchmarkName=character(),
      NumberOfFeatures=integer(), NumberOfFeatureVectors=integer(),
      NumberOfClusters=integer(), UserTime=numeric(), SystemTime=numeric(),
      WallClockTime=numeric(), DateStarted=character(),
      DateFinished=character(), stringsAsFactors=FALSE)

   # Make sure needed parameters exist   
   parameterCheck <- tryCatch({
      cat(sprintf("Running clustering microbenchmark: %s\n", benchmarkParameters$benchmarkName))
      cat(sprintf("Microbenchmark description: %s\n", benchmarkParameters$benchmarkDescription))
      allocator <- match.fun(benchmarkParameters$allocatorFunction)
      benchmark <- match.fun(benchmarkParameters$benchmarkFunction)

      numberOfTrials <- benchmarkParameters$numberOfTrials
      numberOfWarmupTrials <- benchmarkParameters$numberOfWarmupTrials
      benchmarkName <- benchmarkParameters$benchmarkName
      csvResultsBaseFileName <- benchmarkParameters$benchmarkName
      csvResultsFileName <- file.path(resultsDirectory,
         paste(csvResultsBaseFileName, "_", runIdentifier, ".csv", sep=""))

      if (length(numberOfTrials) != length(numberOfWarmupTrials)) {
         stop(sprintf("ERROR: Input checking failed for clustering microbenchmark '%s' -- lengths of numberOfTrials and numberOfWarmupTrials arrays must be equal", benchmarkName))
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
   trialTimes <- rep(NA_real_, maximumNumberOfTrials)
   dim(trialTimes) <- c(maximumNumberOfTrials,1)
   numberOfSuccessfulTrials <- 0
   
   for (i in 1:(numberOfTrials+numberOfWarmupTrials)) {
      cat(sprintf("Running performance trial %d...\n", i))

      allocationSuccessful <- tryCatch({
         RNGkind(kind=RBenchmarkOptions$rng.kind,
            normal.kind=RBenchmarkOptions$rng.normal.kind)
         set.seed(RBenchmarkOptions$rng.seed,
            kind=RBenchmarkOptions$rng.kind,
            normal.kind=RBenchmarkOptions$rng.normal.kind)
         kernelParameters <- allocator(benchmarkParameters)
         numberOfFeatures <- kernelParameters$numberOfFeatures
         numberOfFeatureVectors <- kernelParameters$numberOfFeatureVectors
         numberOfClusters <- kernelParameters$numberOfClusters
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

      if (i > numberOfWarmupTrials) {
         numberOfSuccessfulTrials <- numberOfSuccessfulTrials + 1 
         resultsFrame[nrow(resultsFrame)+1, ] <- list(
            benchmarkName, as.integer(numberOfFeatures),
            as.integer(numberOfFeatureVectors), as.integer(numberOfClusters),
            userTime, systemTime, wallClockTime, dateStarted, dateFinished)
      }
       
      remove(kernelParameters)
      invisible(gc())

      if (i > numberOfWarmupTrials) {
         trialTimes[i-numberOfWarmupTrials,1] <- wallClockTime
      }

      cat(sprintf("done: %f(sec)\n", wallClockTime))
   }

   averageWallClockTime <- ComputeAverageTime(numberOfSuccessfulTrials,
      trialTimes) 
   standardDeviation <- ComputeStandardDeviation(numberOfSuccessfulTrials,
      trialTimes)
   WriteClusteringPerformanceResultsCsv(numberOfThreads, numberOfFeatures,
      numberOfFeatureVectors, numberOfClusters, averageWallClockTime,
      standardDeviation, csvResultsFileName)

   PrintClusteringMicrobenchmarkResults(benchmarkName, numberOfThreads,
      numberOfFeatures, numberOfFeatureVectors, numberOfClusters,
      numberOfSuccessfulTrials, trialTimes, averageWallClockTime,
      standardDeviation)

   return (resultsFrame)
}
