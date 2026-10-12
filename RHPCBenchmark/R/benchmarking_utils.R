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

#' Retrieves the value of an environment variable referenced by another
#' environment variable
#'
#' \code{GetConfigurableEnvParameters} returns the value of the environment
#' variable referenced by the argument \code{configurableVariable} which
#' is also an environment variable
#' 
#' This function takes the argument \code{configurableVariable} which
#' contains the name of an environment variable whose value is an
#' environment variable referencing a value to be returned by this
#' function.
#'
#' @param configurableVariable a string parameter containing the name
#'   of an environment variable which itself references another 
#'   environment variable
#' @return the value of the environment variable referenced by the
#'   environment variable specified in the \code{configurableVariable}
#'   parameter
GetConfigurableEnvParameter <- function(configurableVariable) {
   # Get the name of the wanted environment variable from the
   # configurable environment variable.
   envVariable <- Sys.getenv(configurableVariable)

   if (envVariable == "") {
      stop(sprintf("ERROR: Configurable environment variable '%s' is undefined", configurableVariable))
   } else {
      value <- Sys.getenv(envVariable)

      if (value == "") {
         stop(sprintf("ERROR: Environment variable '%s' is undefined", envVariable))
      }
   }

   return(value)
}


#' Retrieves the number of threads from the environment
#'
#' \code{GetNumberOfThreads} retrieves from the environment the number of
#' threads kernels are intended to be executed with
#' 
#' This function retrieves the number of threads kernels are intended to be
#' microbenchmarked with.  The number of threads is assumed to be stored
#' in an environment variable which this function retrieves.
#'
#' @return the number of threads retrieved from the environment
GetNumberOfThreads <- function() {
   success <- tryCatch({
      numberOfThreadsStr <- GetConfigurableEnvParameter(RBenchmarkOptions$numThreadsVariable)
      numberOfThreads <- strtoi(numberOfThreadsStr)
      TRUE
   }, warning = function(war) {
      msg <- sprintf("ERROR: GetNumberOfThreads caught a warning -- %s", war)
      write(msg, stderr())
      return(FALSE)
   }, error = function(err) {
      msg <- sprintf("ERROR: GetNumberOfThreads caught an error -- %s", err)
      write(msg, stderr())
      return(FALSE)
   })

   if (!success) {   
      quit(status=1)
   }

   return(numberOfThreads)
}


#' Prints results of a dense matrix microbenchmark 
#'
#' \code{PrintDenseMatrixMicrobenchmarkResults} prints performance results for a
#' dense matrix microbenchmark to standard output in a format that is easily
#' human readable
#' 
#' This function prints the performance results obtained by a dense matrix
#' microbenchmark for matrices of various dimensions.  The results include
#' summary statistics for each matrix tested.  The summary statistics
#' include the minimum, maximum, average, and standard deviation of the wall
#' clock times obtained by the performance trials with respect to each matrix
#' tested.
#'
#' @param benchmarkName character string specifying the name of the
#'   microbenchmark
#' @param numberOfThreads the number of threads all of the performance trials
#'   were conducted with
#' @param dimensionParameters an integer vector specifying the dimension
#'   parameters the microbenchmark uses to define the matrix dimensions to be
#'   tested with; length is assumed to be greater than zero
#' @param numberOfSuccessfulTrials an integer vector specifying the number of
#'   performance trials that were successfully performed for each matrix tested
#' @param trialTimes a real matrix with each column containing the run times
#'   of all of the successful performance trials associated with a particular
#'   matrix.  The number of valid entries in each column are specified by the
#'   entries in the \code{numberOfSuccessfulTrials} vector
#' @param averageWallClockTimes a vector of average wall clock times computed
#'   for each matrix tested during the performance trials
#' @param standardDeviations a vector of standard deviations of the wall clock
#'   times obtained for each matrix tested during the performance trials
PrintDenseMatrixMicrobenchmarkResults <- function(benchmarkName,
      numberOfThreads, dimensionParameters, numberOfSuccessfulTrials,
      trialTimes, averageWallClockTimes, standardDeviations) {
   numberOfDimensions <- length(dimensionParameters)

   cat(  "-------------------------------------------------------------------------------\n")
   cat(sprintf(" Timings summary for microbenchmark: %s\n", benchmarkName))
   cat(  "-------------------------------------------------------------------------------\n")
   cat(sprintf(" numberOfThreads=%s\n", numberOfThreads))
   cat(  "-------------------------------------------------------------------------------\n")
   cat(c(" N         | Number of | Min Wall    | Max Wall    | Average     | Standard    \n"))
   cat(c("           | Trials    | Time        | Time        | Wall Time   | deviation   \n"))
   cat(  "-------------------------------------------------------------------------------\n")

   for (i in 1:numberOfDimensions) {
      if (numberOfSuccessfulTrials[i] > 0) {
         cat(sprintf(" %-10d  %-10d  %-12.6e  %-12.6e  %-12.6e  %-12.6e\n", dimensionParameters[i], numberOfSuccessfulTrials[i], min(trialTimes[1:numberOfSuccessfulTrials[i], i]), max(trialTimes[1:numberOfSuccessfulTrials[i], i]), averageWallClockTimes[i], standardDeviations[i]))
      } else {
         cat(" There were no successful performance trials for this microbenchmark.\n")
         cat(" Please look for warnings or errors prior to this report.\n")
      }
   }

   cat(  "-------------------------------------------------------------------------------\n\n")

}


#' Prints results of a sparse matrix microbenchmark 
#'
#' \code{PrintSparseMatrixMicrobenchmarkResults} prints performance results for
#' a sparse matrix microbenchmark to standard output in a format that is easily
#' human readable
#' 
#' This function prints the run time performance results obtained by a sparse
#' matrix microbenchmark for matrices of various dimensions.  The summary
#' statistics include the minimum, maximum, average, and standard deviation of
#' the wall clock times obtained by the performance trials with respect to each
#' matrix tested.
#'
#' @param benchmarkName character string specifying the name of the
#'   microbenchmark
#' @param numberOfThreads the number of threads all of the performance trials
#'   were conducted with
#' @param numberOfRows the number of expected rows in the matrix; assumed to
#'   be greater than zero
#' @param numberOfColumns the number of expected columns in the matrix; assumed
#'   to be greater than zero
#' @param numberOfNonzeros the number of non-zero elements in the matrix
#' @param numberOfSuccessfulTrials an integer vector specifying the number of
#'   performance trials that were successfully performed for each matrix tested
#' @param trialTimes a real matrix with each column containing the run times
#'   of all of the successful performance trials associated with a particular
#'   matrix.  The number of valid entries in each column are specified by the
#'   entries in the \code{numberOfSuccessfulTrials} vector
#' @param averageWallClockTimes a vector of average wall clock times computed
#'   for each matrix tested during the performance trials
#' @param standardDeviations a vector of standard deviations of the wall clock
#'   times obtained for each matrix tested during the performance trials
PrintSparseMatrixMicrobenchmarkResults <- function(benchmarkName,
     numberOfThreads, numberOfRows, numberOfColumns, numberOfNonzeros, 
     numberOfSuccessfulTrials, trialTimes, averageWallClockTimes,
     standardDeviations) {

   cat(  "-------------------------------------------------------------------------------------------------------\n")
   cat(sprintf(" Timings summary for microbenchmark: %s\n", benchmarkName))
   cat(  "-------------------------------------------------------------------------------------------------------\n")
   cat(sprintf(" numberOfThreads=%s\n", numberOfThreads))
   cat(  "-------------------------------------------------------------------------------------------------------\n")
   cat(c(" Number of | Number of | Number of | Number of | Min Wall    | Max Wall    | Average     | Standard    \n"))
   cat(c(" Rows      | Columns   | Trials    | Nonzeros  | Time        | Time        | Wall Time   | deviation   \n"))
   cat(  "-------------------------------------------------------------------------------------------------------\n")

   for (i in 1:length(numberOfRows)) {
      if (numberOfSuccessfulTrials[i] > 0) {
         cat(sprintf(" %-10d  %-10d  %-10d  %-10d  %-12.6e  %-12.6e  %-12.6e  %-12.6e\n",
            numberOfRows, numberOfColumns, numberOfNonzeros,
            numberOfSuccessfulTrials[i],
            min(trialTimes[1:numberOfSuccessfulTrials[i], i]),
            max(trialTimes[1:numberOfSuccessfulTrials[i], i]),
            averageWallClockTimes[i], standardDeviations[i]))
      } else {
         cat(" There were no successful performance trials for this microbenchmark.\n")
         cat(" Please look for warnings or errors prior to this report.\n")
      }
   }

   cat(  "-------------------------------------------------------------------------------------------------------\n\n")
}


#' Prints results of a clustering for machine learning microbenchmark 
#'
#' \code{PrintClusteringMicrobenchmarkResults} prints performance results
#' for a clustering for machine learning microbenchmark to standard output in a
#' format that is easily human readable
#' 
#' This function prints the performance results obtained by a clustering for
#' machine learning microbenchmark.  Summary run time performance statistics for
#' each clustering data set tested are computed and printed.  The summary
#' statistics include the minimum, maximum, average, and standard deviation of
#' the wall clock times obtained by the performance trials with respect to each
#' data tested.
#'
#' @param benchmarkName character string specifying the name of the
#'   microbenchmark
#' @param numberOfThreads the number of threads all of the performance trials
#'   were conducted with
#' @param numberOfFeatures the number of features, i.e. the dimension of the
#'   feature vector
#' @param numberOfFeatureVectors the number of feature vectors in the data set
#' @param numberOfClusters the number of clusters in the data set
#' @param numberOfSuccessfulTrials an integer vector specifying the number of
#'   performance trials that were successfully performed for each data set
#' @param trialTimes a real matrix with each column containing the run times
#'   of all of the successful performance trials associated with a particular
#'   data set.  The number of valid entries in each column are specified by the
#'   entries in the \code{numberOfSuccessfulTrials} vector
#' @param averageWallClockTimes a vector of average wall clock times computed
#'   for each matrix tested during the performance trials
#' @param standardDeviations a vector of standard deviations of the wall clock
#'   times obtained for each matrix tested during the performance trials
#' @family print machine learning performance results
PrintClusteringMicrobenchmarkResults <- function(benchmarkName,
     numberOfThreads, numberOfFeatures, numberOfFeatureVectors,
     numberOfClusters, numberOfSuccessfulTrials, trialTimes,
     averageWallClockTimes, standardDeviations) {

   cat(  "-------------------------------------------------------------------------------------------------------\n")
   cat(sprintf(" Timings summary for microbenchmark: %s\n", benchmarkName))
   cat(  "-------------------------------------------------------------------------------------------------------\n")
   cat(sprintf(" numberOfThreads=%s\n", numberOfThreads))
   cat(  "-------------------------------------------------------------------------------------------------------\n")
   cat(c(" Number of | Number of | Number of | Number of | Min wall    | Max Wall    | Average     | Standard\n"))
   cat(c(" Features  | Feature   | Clusters  | Trials    | Time        | Time        | Wall Time   | Deviation\n"))
   cat(c("           | Vectors   |           |           |             |             |             | \n"))
   cat(  "-------------------------------------------------------------------------------------------------------\n")

   for (i in 1:length(numberOfFeatureVectors)) {
      if (numberOfSuccessfulTrials[i] > 0) {
         cat(sprintf(" %-10d  %-10d  %-10d  %-10d  %-12.6e  %-12.6e  %-12.6e  %-12.6e\n",
            numberOfFeatures, numberOfFeatureVectors, numberOfClusters,
            numberOfSuccessfulTrials[i],
            min(trialTimes[1:numberOfSuccessfulTrials[i], i]),
            max(trialTimes[1:numberOfSuccessfulTrials[i], i]),
            averageWallClockTimes[i], standardDeviations[i]))
      } else {
         cat(" There were no successful performance trials for this microbenchmark.\n")
         cat(" Please look for warnings or errors prior to this report.\n")
      }
   }

   cat(  "-------------------------------------------------------------------------------------------------------\n\n")
}


#' Appends dense matrix performance test results to a file in CSV format
#'
#' \code{WriteDenseMatrixPerformanceResultsCsv} appends performance results
#' for a single dense matrix microbenchmark to a CSV file
#' 
#' This function appends to a CSV file the performance results obtained by a
#' single dense matrix performance microbenchmark conducted for a specific
#' matrix.  If the CSV file does not exist, header information is printed on the
#' first line to describe the subsequent entries.  Each entry consists of the
#' dimension parameter used to specify the dimensions of the matrix, the average
#' of the wall clock times obtained for the performance trials, the standard
#' deviation of the performance trial wall clock times, and the number of
#' threads the performance trials conducted with.
#'
#' @param numberOfThreads the number of threads all of the performance trials
#'   were conducted with
#' @param dimensionParameter an integer vector specifying the dimension
#'   parameter the microbenchmark uses to define the dimension of the test
#'   matrix
#' @param averageWallClockTime average wall clock time computed for the matrix
#'   tested during the performance trials
#' @param standardDeviation standard deviation of the wall clock times obtained
#'   for the performance trials
#' @param csvResultsFileName the CSV results file the performance result will be
#'   appended to
WriteDenseMatrixPerformanceResultsCsv <- function(numberOfThreads,
      dimensionParameter, averageWallClockTime, standardDeviation,
      csvResultsFileName) {
   if (csvResultsFileName != "" && !file.exists(csvResultsFileName)) {
      cat("Dimension", "Avg. Wall Clock Time", "Std. Dev", "Number of Threads\n", file=csvResultsFileName, sep=",", append=FALSE)
   }

   if (csvResultsFileName != "") {
      cat(sprintf("%d,%12.6e,%12.6e,%d\n", dimensionParameter, averageWallClockTime, standardDeviation, numberOfThreads), file=csvResultsFileName, append=TRUE)
   }
}


#' Appends sparse matrix performance test results to a file in CSV format
#'
#' \code{WriteSparseMatrixPerformanceResultsCsv} appends performance results
#' for a single sparse matrix performance test to a CSV file.
#' 
#' This function appends the performance results obtained by a single sparse
#' matrix microbenchmark conducted for a specific matrix.
#' If the CSV file does not exist, header information is printed on the first
#' line to describe the subsequent entries.  Each entry consists of the
#' dimension parameter used to specify the dimensions of the matrix, the
#' average of the wall clock times obtained for the performance trials, the
#' standard deviation of the performance trial wall clock times, and the number
#' of threads the performance trials were conducted with.
#'
#' @param numberOfThreads the number of threads all of the performance trials
#'   were conducted with
#' @param numberOfRows the number of rows in the matrix
#' @param numberOfColumns the number of columns in the matrix
#' @param numberOfNonzeros the number of non-zero elements in the matrix
#' @param averageWallClockTime average wall clock time computed for the matrix
#'   tested during the performance trials
#' @param standardDeviation standard deviation of the wall clock times obtained
#'   for the performance trials
#' @param csvResultsFileName the CSV results file the performance result will be
#'   appended to
WriteSparseMatrixPerformanceResultsCsv <- function(numberOfThreads,
   numberOfRows, numberOfColumns, numberOfNonzeros, averageWallClockTime,
   standardDeviation, csvResultsFileName) {

   if (csvResultsFileName != "" && !file.exists(csvResultsFileName)) {
      cat("Num. Rows", "Num. Cols", "Num. Nonzeros", "Avg. Wall Clock Time", "Std. Dev", "Number of Threads\n", file=csvResultsFileName, sep=",", append=FALSE)
   }

   if (csvResultsFileName != "") {
      cat(sprintf("%d,%d,%d,%12.6e,%12.6e,%d\n", numberOfRows, numberOfColumns,
         numberOfNonzeros, averageWallClockTime, standardDeviation,
         numberOfThreads), file=csvResultsFileName, append=TRUE)
   }
}


#' Appends performance test results of a clustering microbenchmark to a file in
#' CSV format 
#'
#' \code{WriteClusteringPerformanceResultsCsv} appends performance results
#' for a clustering for machine learning microbenchmark to a CSV file.
#' 
#' This function appends the performance results obtained by a single clustering
#' for machine learning microbenchmark conducted with a specific data set.
#' If the CSV file does not exist, header information is printed on the first
#' line to describe the subsequent entries.  Each entry includes the
#' number of features, number of feature vectors, and number of clusters in the
#' data set.  The performance results included in each entry are the average of
#' the wall clock times obtained for the performance trials, the standard
#' deviation of the performance trial wall clock times, and the number of
#' threads the performance trials were conducted with.
#'
#' @param numberOfThreads the number of threads all of the performance trials
#'   were conducted with
#' @param numberOfFeatures the number of features, i.e. the dimension of the
#'   feature vector
#' @param numberOfFeatureVectors the number of feature vectors in the data set
#' @param numberOfClusters the number of clusters in the data set
#' @param averageWallClockTime average wall clock time computed for the data
#'   set tested during the performance trials
#' @param standardDeviation standard deviation of the wall clock times obtained
#'   for the performance trials
#' @param csvResultsFileName the CSV results file the performance result will be
#'   appended to
#' @family write machine learning performance results
WriteClusteringPerformanceResultsCsv <- function(numberOfThreads,
   numberOfFeatures, numberOfFeatureVectors, numberOfClusters,
   averageWallClockTime, standardDeviation, csvResultsFileName) {

   if (csvResultsFileName != "" && !file.exists(csvResultsFileName)) {
      cat("Num. Features", "Num. Feature Vectors", "Num. Clusters", "Avg. Wall Clock Time", "Std. Dev", "Number of Threads\n", file=csvResultsFileName, sep=",", append=FALSE)
   }

   if (csvResultsFileName != "") {
      cat(sprintf("%d,%d,%d,%12.6e,%12.6e,%d\n", numberOfFeatures,
         numberOfFeatureVectors, numberOfClusters, averageWallClockTime,
         standardDeviation, numberOfThreads),
         file=csvResultsFileName, append=TRUE)
   }
}
   

#' Computes the average of a vector of performance trial times
#'
#' \code{ComputeAverageTime} computes the average of a vector of performance
#'   trial times.  The average is computed only over the first
#'   \code{numberOfSuccessfulTrials} elements of the \code{times} vector.
#'
#' @param numberOfSuccessfulTrials the number of successful performance trials
#'   to be averaged over 
#' @param times a vector of wall clock times for the performance trials
ComputeAverageTime <- function(numberOfSuccessfulTrials, times) {
   return(mean(times[1:numberOfSuccessfulTrials]))
}


#' Computes the standard deviation of a vector of performance trial times
#'
#' \code{ComputeStandardDeviation} computes the standard deviation of a vector
#'   of performance trial times.  The standard deviation is computed only over
#'   the first \code{numberOfSuccessfulTrials} elements of the \code{times}
#'   vector.
#'
#' @param numberOfSuccessfulTrials the number of successful performance trials
#'   over which the standard deviation will be computed
#' @param times a vector of wall clock times for the performance trials
ComputeStandardDeviation <- function(numberOfSuccessfulTrials, times) {
   return(sqrt(stats::var(times[1:numberOfSuccessfulTrials])))
}
