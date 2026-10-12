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

#' This class specifies a dense matrix microbenchmark.
#' 
#' @name DenseMatrixMicrobenchmark
#' @field active a logical indicating whether the microbenchmark is to be
#'   executed (TRUE) or not (FALSE).
#' @field benchmarkName a character string that is the name of the
#'   microbenchmark.
#' @field benchmarkDescription a character string describing the microbenchmark.
#' @field dimensionParameters an integer vector specifying the dimension
#'   parameters the microbenchmark uses to define the matrix dimensions to be
#'   tested with.
#' @field numberOfTrials an integer vector specifying the number of performance
#'   trials conducted for each matrix to be tested.  Must be the same length as
#'   \code{dimensionParameters}.
#' @field numberOfWarmupTrials an integer vector specifying the number of warmup
#'   trials to be performed for each matrix to be tested.
#' @field allocatorFunction the function that allocates and initializes input to
#'   the benchmark function.  The function takes a
#'   \code{DenseMatrixMicrobenchmark} object and an integer index indicating
#'   which matrix dimension parameter from \code{dimensionParameters} should
#'   be used to generate the matrix. 
#' @field benchmarkFunction the benchmark function which executes the
#'   functionality to be timed.  The function takes a
#'   \code{DenseMatrixMicrobenchmark} and a list of kernel parameters
#'   returned by the allocator function.
methods::setRefClass(
   "DenseMatrixMicrobenchmark",
   fields = list(
      active = "logical",
      benchmarkName = "character",
      benchmarkDescription = "character",
      dimensionParameters = "integer",
      numberOfTrials = "integer",
      numberOfWarmupTrials = "integer",
      allocatorFunction = "function",
      benchmarkFunction = "function"
   )
)


#' This class specifies a sparse matrix microbenchmark.
#'
#' @name SparseMatrixMicrobenchmark
#' @field active a logical indicating whether the microbenchmark is to be
#'   executed (TRUE) or not (FALSE).
#' @field benchmarkName a character string that is the name of the
#'   microbenchmark.
#' @field benchmarkDescription a character string describing the microbenchmark.
#' @field matrixObjectName a character string specifying the name of the sparse
#'   matrix object that is input to the benchmark; the object must be stored in
#'   the R data file with name \code{matrixObjectName}.RData
#'   Setting the field to NA_character_ indicates that the test data will
#'   be generated dynamically by the function given in the
#'   \code{allocatorFunction} field instead of read from a data file.
#' @field numberOfRows an integer specifying the expected number of rows in the
#'   input sparse matrix.
#' @field numberOfColumns an integer specifying the expected number of columns
#'   in the input sparse matrix.
#' @field numberOfNonzeros an integer specifying the expected number of nonzeros
#'   in the input sparse matrix.
#' @field numberOfTrials an integer vector specifying the number of performance
#'   trials conducted for each matrix to be tested.
#' @field numberOfWarmupTrials an integer vector specifying the number of warmup
#'   trials to be performed for each matrix to be tested.
#' @field allocatorFunction the function that allocates and initializes input
#'   to the benchmark function.  The function takes a
#'   \code{SparseMatrixMicrobenchmark} object and an integer index indicating
#'   which matrix parameter from \code{numberOfRows}, \code{numberOfColumns},
#'   and \code{numberOfNonzeros} should be used to generate the matrix. 
#' @field benchmarkFunction the benchmark function which executes the
#'   functionality to be timed.  The function takes a
#'   \code{SparseMatrixMicrobenchmark} and a list of kernel parameters
#'   returned by the allocator function.
methods::setRefClass(
   "SparseMatrixMicrobenchmark",
   fields = list(
      active = "logical",
      benchmarkName = "character",
      benchmarkDescription = "character",
      matrixObjectName = "character",
      numberOfRows = "integer",
      numberOfColumns = "integer",
      numberOfNonzeros = "integer",
      numberOfTrials = "integer",
      numberOfWarmupTrials = "integer",
      allocatorFunction = "function",
      benchmarkFunction = "function"
   )
)


#' This class specifies a clustering for machine learning microbenchmark.
#'
#' @name ClusteringMicrobenchmark
#' @field active a logical indicating whether the microbenchmark is to be
#'   executed (TRUE) or not (FALSE).
#' @field benchmarkName a character string that is the name of the
#'   microbenchmark.
#' @field benchmarkDescription a character string describing the microbenchmark.
#' @field dataObjectName a character string specifying the name of the data
#'   object that is input to the benchmark; the object must be stored in
#'   the R data file with the same base name and a \code{.RData} extension.
#'   Setting the field to \code{NA_character_} indicates that the test data will
#'   be dynamically generated by the function given in the
#'   \code{allocatorFunction} field instead of read from a data file.
#' @field numberOfFeatures the number features; this value must match the
#'   number of features in the data set given by the field \code{dataObjectName}
#'   unless the field is populated with \code{NA_character_}.
#' @field numberOfClusters the number of clusters in the data set; this value
#'   must match the number of clusters in the data set given by the field
#'   \code{dataObjectName} unless the field is populated with
#'   \code{NA_character_}.
#' @field numberOfFeatureVectorsPerCluster the number of feature vectors per
#'   cluster; this value must match the number of clusters in the data set given
#'   by the field \code{dataObjectName} unless the field is populated with
#'   \code{NA_character_}.
#' @field numberOfTrials an integer specifying the number of performance
#'   trials conducted on the data set to be tested.
#' @field numberOfWarmupTrials an integer specifying the number of warmup
#'   trials to be conducted on the data set.
#' @field allocatorFunction the function that allocates and initializes input
#'   to the benchmark function.  The function takes a
#'   \code{ClusteringMicrobenchmark} object.  For clustering benchmarks, the
#'   allocator function should return a list containing the following items:
#'   \describe{
#'     \item{featureVectors}{a matrix, the rows of which are the feature
#'       vectors}
#'     \item{numberOfFeatures}{an integer indicating the number of features}
#'     \item{numberOfFeatureVectors}{an integer indicating the number of feature
#'       vectors}
#'     \item{numberOfClusters}{an integer indicating the number of clusters in
#'       the data set}
#'   }
#' @field benchmarkFunction the benchmark function which executes the
#'   functionality to be timed.  The function takes a
#'   \code{SparseMatrixMicrobenchmark} and a list of kernel parameters
#'   returned by the allocator function.
methods::setRefClass(
   "ClusteringMicrobenchmark",
   fields = list(
      active = "logical",
      benchmarkName = "character",
      benchmarkDescription = "character",
      dataObjectName = "character",
      numberOfFeatures = "integer",
      numberOfClusters = "integer",
      numberOfFeatureVectorsPerCluster = "integer",
      numberOfTrials = "integer",
      numberOfWarmupTrials = "integer",
      allocatorFunction = "function",
      benchmarkFunction = "function"
   )
)


