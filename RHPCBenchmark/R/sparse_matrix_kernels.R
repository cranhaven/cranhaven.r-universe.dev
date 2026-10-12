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

#' Allocates and initializes input to the matrix-vector multiplication sparse
#' matrix kernel microbenchmarks
#'
#' \code{SparseMatrixVectorAllocator} allocates and initializes the sparse
#' matrix and vector that are inputs to the sparse matrix kernel for the
#' purposes of conducting a single performance trial with the
#' \code{SparseMatrixVectorMicrobenchmark} function.  The matrix and vector
#' are populated and returned in the \code{kernelParameters} list.
#'
#' @param benchmarkParameters an object of type
#'   \code{\link{SparseMatrixMicrobenchmark}} specifying various parameters
#'   needed to generate input for the sparse matrix kernel.
#' @param index an integer index indicating the dimensions of the matrix or
#'   vector data to be generated as input for the sparse matrix kernel.
#' @return a list containing the matrices or vectors to be input for the
#'   sparse matrix kernel for which a single performance trial is to be
#'   conducted.
#'
#' @export
SparseMatrixVectorAllocator <- function(benchmarkParameters, index) {
   # Create list of kernel parameters
   kernelParameters <- list()
   kernelParameters$A <- get(benchmarkParameters$matrixObjectName)

   # Make sure expected matrix dimensions agree
   if (nrow(kernelParameters$A) != benchmarkParameters$numberOfRows[index]) {
      warning("Actual number of rows in sparse matrix does not match expected number of rows in numberOfRows")
   } else if (ncol(kernelParameters$A) != benchmarkParameters$numberOfColumns[index]) {
      warning("Actual number of columns in sparse matrix does not match expected number of columns in numberOfColumns")
   }

   s <- ncol(kernelParameters$A)
   kernelParameters$x <- matrix(stats::rnorm(s), nrow=s, ncol=1)
   return (kernelParameters)
}

#' Conducts a single performance trial with the matrix-vector multiplication
#' sparse matrix kernel
#'
#' \code{SparseMatrixVectorMicrobenchmark} conducts a single performance trial
#' of the matrix-vector multiplication sparse matrix kernel for the matrix given
#' in the \code{kernelParameters} parameter.  The function times the single
#' function call \code{kernelParameters$A \%*\% kernelParameters$b}.
#'
#' @param benchmarkParameters an object of type
#'   \code{\link{SparseMatrixMicrobenchmark}} specifying various parameters
#'   for microbenchmarking the sparse matrix kernel
#' @param kernelParameters a list of matrices or vectors to be used as input to
#'   the sparse matrix kernel
#' @return a vector containing the user, system, and elapsed performance
#'   timings in that order
#'
#' @examples
#' \dontrun{
#' # Allocate input to the matrix-vector microbenchmark for the first Laplacian
#' # matrix
#' microbenchmarks <- GetSparseMatrixVectorDefaultMicrobenchmarks()
#' kernelParameters <- SparseMatrixVectorAllocator(
#'    microbenchmarks[["matvec_laplacian7pt_100"]], 1)
#' # Execute the microbenchmark
#' timings <- SparseMatrixVectorMicrobenchmark(
#'    microbenchmarks[["matvec_laplacian7pt_100"]], kernelParameters)
#' }
#'
#' @export
SparseMatrixVectorMicrobenchmark <- function(benchmarkParameters, kernelParameters) {
   timings <- system.time({b <- kernelParameters$A %*% kernelParameters$x})
   return (timings)
}


#' Allocates and initializes input to the Cholesky factorization sparse
#' matrix kernel microbenchmarks
#'
#' \code{SparseCholeskyAllocator} allocates and initializes the sparse
#' matrix that is input to the sparse matrix kernel for the
#' purposes of conducting a single performance trial with the
#' \code{SparseCholeskyMicrobenchmark} function.  The matrix is populated
#' and returned in the \code{kernelParameters} list.
#'
#' @inheritParams SparseMatrixVectorAllocator
#'
#' @export
SparseCholeskyAllocator <- function(benchmarkParameters, index) {
   # Create list of kernel parameters
   kernelParameters <- list()
   kernelParameters$A <- get(benchmarkParameters$matrixObjectName)

   # Make sure expected matrix dimensions agree
   if (nrow(kernelParameters$A) != benchmarkParameters$numberOfRows[index]) {
      warning("Actual number of rows in sparse matrix does not match expected number of rows in numberOfRows")
   } else if (ncol(kernelParameters$A) != benchmarkParameters$numberOfColumns[index]) {
      warning("Actual number of columns in sparse matrix does not match expected number of columns in numberOfColumns")
   }

   return (kernelParameters)
}

#' Conducts a single performance trial with the Cholesky factorization
#' sparse matrix kernel
#'
#' \code{SparseMatrixVectorMicrobenchmark} conducts a single performance trial
#' of the Cholesky factorization sparse matrix kernel for the matrix given in
#' the \code{kernelParameters} parameter.  The function times the single
#' function call \code{kernelParameters$A \%*\% kernelParameters$b}.
#'
#' @param benchmarkParameters an object of type
#'   \code{\link{SparseMatrixMicrobenchmark}} specifying various parameters
#'   for microbenchmarking the sparse matrix kernel
#' @param kernelParameters a list of matrices or vectors to be used as input to
#'   the sparse matrix kernel
#' @return a vector containing the user, system, and elapsed performance
#'   timings in that order
#'
#' @inheritParams SparseMatrixVectorMicrobenchmark
#'
#' @examples
#' \dontrun{
#' # Allocate input to the Cholesky factorization microbenchmark for the
#' # ct20stif matrix
#' microbenchmarks <- GetSparseCholeskyDefaultMicrobenchmarks()
#' kernelParameters <- SparseCholeskyAllocator(microbenchmarks[["cholesky_ct20stif"]], 1)
#' # Execute the microbenchmark
#' timings <- SparseCholeskyMicrobenchmark(
#'    microbenchmarks[["choleksy_ct20stif"]], kernelParameters)
#' }
#'
#' @export
SparseCholeskyMicrobenchmark <- function(benchmarkParameters, kernelParameters) {
   timings <- system.time({b <- Matrix::Cholesky(kernelParameters$A)})
   return (timings)
}


#' Allocates and initializes input to the LU factorization sparse matrix kernel
#' microbenchmarks
#'
#' \code{SparseLuAllocator} allocates and initializes the sparse matrix that is
#' input to the sparse matrix kernel for the purposes of conducting a single
#' performance trial with the \code{SparseCholeskyMicrobenchmark} function.  The
#' matrix is populated and returned in the \code{kernelParameters} list.
#'
#' @inheritParams SparseMatrixVectorAllocator
#'
#' @export
SparseLuAllocator <- function(benchmarkParameters, index) {
   # Create list of kernel parameters
   kernelParameters <- list()
   kernelParameters$A <- get(benchmarkParameters$matrixObjectName)
   # The sparse matrix objects can be altered by reference.
   # When lu() is called on the sparse matrix, it will attach
   # the L and U factors to the input matrix under the 'factors'
   # field.  Thus, the factorization will only be perfored for the
   # first performance trial unless the 'factors' field is set to
   # the empty list.
   kernelParameters$A@factors <- list()

   # Make sure expected matrix dimensions agree
   if (nrow(kernelParameters$A) != benchmarkParameters$numberOfRows[index]) {
      warning("Actual number of rows in sparse matrix does not match expected number of rows in numberOfRows")
   } else if (ncol(kernelParameters$A) != benchmarkParameters$numberOfColumns[index]) {
      warning("Actual number of columns in sparse matrix does not match expected number of columns in numberOfColumns")
   }

   return (kernelParameters)
}

#' Conducts a single performance trial with the LU factorization sparse matrix
#' kernel
#'
#' \code{SparseMatrixVectorMicrobenchmark} conducts a single performance trial
#' of the Cholesky factorization sparse matrix kernel for the matrix given
#' in the \code{kernelParameters} parameter.  The function times the single
#' function
#' call \code{kernelParameters$A \%*\% kernelParameters$b}.
#'
#' @inheritParams SparseMatrixVectorMicrobenchmark
#'
#' @examples
#' \dontrun{
#' # Allocate input to the LU factorization microbenchmark for the
#' # circuit5M_dc matrix
#' microbenchmarks <- GetSparseLuDefaultMicrobenchmarks()
#' kernelParameters <- SparseLuAllocator(microbenchmarks[["lu_circuit5M_dc"]], 1)
#' # Execute the microbenchmark
#' timings <- SparseLuMicrobenchmark(
#'    microbenchmarks[["lu_circuit5M_dc"]], kernelParameters)
#' }
#'
#' @export
SparseLuMicrobenchmark <- function(benchmarkParameters, kernelParameters) {
   timings <- system.time({b <- Matrix::lu(kernelParameters$A)})
   return (timings)
}



#' Allocates and initializes input to the QR factorization sparse matrix kernel
#' microbenchmarks
#'
#' \code{SparseQrAllocator} allocates and initializes the sparse matrix that is
#' input to the sparse matrix kernel for the purposes of conducting a single
#' performance trial with the \code{SparseQrMicrobenchmark} function.  The
#' matrix is populated and returned in the \code{kernelParameters} list.
#'
#' @inheritParams SparseMatrixVectorAllocator
#'
#' @export
SparseQrAllocator <- function(benchmarkParameters, index) {
   # Create list of kernel parameters
   kernelParameters <- list()
   kernelParameters$A <- get(benchmarkParameters$matrixObjectName)

   # Make sure expected matrix dimensions agree
   if (nrow(kernelParameters$A) != benchmarkParameters$numberOfRows[index]) {
      warning("Actual number of rows in sparse matrix does not match expected number of rows in numberOfRows")
   } else if (ncol(kernelParameters$A) != benchmarkParameters$numberOfColumns[index]) {
      warning("Actual number of columns in sparse matrix does not match expected number of columns in numberOfColumns")
   }

   return (kernelParameters)
}

#' Conducts a single performance trial with the QR factorization sparse matrix
#' kernel
#'
#' \code{SparseQrVectorMicrobenchmark} conducts a single performance trial
#' of the QR factorization sparse matrix kernel for the matrix given
#' in the \code{kernelParameters} parameter.  The function times the single
#' function call \code{qr(kernelParameters$A)}.
#'
#' @inheritParams SparseMatrixVectorMicrobenchmark
#'
#' @examples
#' \dontrun{
#' # Allocate input to the QR factorization microbenchmark for the
#' # Maragal_6 matrix
#' microbenchmarks <- GetSparseQrDefaultMicrobenchmarks()
#' kernelParameters <- SparseQrAllocator(microbenchmarks[["qr_Maragal_6"]], 1)
#' # Execute the microbenchmark
#' timings <- SparseQrMicrobenchmark(
#'    microbenchmarks[["qr_Maragal_6"]], kernelParameters)
#' }
#'
#' @export
SparseQrMicrobenchmark <- function(benchmarkParameters, kernelParameters) {
   timings <- system.time({b <- Matrix::qr(kernelParameters$A)})
   return (timings)
}
