#' RHPCBenchmark: A package for performance testing intrinsic R functionality
#' and established packages relevant to high-performance computing
#'
#' The benchmarks are divided into three categories: dense matrix linear
#' algebra kernels, sparse matrix linear algebra kernels, and machine learning
#' functionality.  All of the dense linear algebra kernels are implemented
#' around BLAS or LAPACK interfaces.  The sparse linear algebra kernels are
#' members of the R Matrix library.  The machine learning benchmarks currently
#' only cover variants of K-means functionality for clustering using the
#' \code{cluster} package.  The dense matrix linear algebra kernels, sparse
#' matrix linear algebra kernels, and machine learning functions that are
#' benchmarked are all part of the R interpreter's intrinsic functionality or
#' packages included the with the R programming environment standard
#' distributions from CRAN.
#'
#' For fast performance of the dense matrix kernels, it is crucial to link
#' the R programming environment with optimized BLAS and LAPACK libraries.
#' It is also important to have substantial amounts of memory (16GB minimum)
#' to run most of the microbenchmarks.  If any of the microbenchmarks fails
#' to run in a timely manner or fails due to memory constraints, the matrix
#' sizes and number of performance trials per matrix can be adjusted.  See
#' the documentation for top-level benchmark functions and the microbenchmark
#' definition classes listed below for information on how to configure the
#' individual microbenchmarks.
#'
#' @section Top-level benchmark functions:
#' \describe{
#'    \item{\code{\link{RunDenseMatrixBenchmark}}}{Executes the dense matrix
#'      microbenchmarks}
#'    \item{\code{\link{RunSparseMatrixBenchmark}}}{Executes the sparse matrix
#'      microbenchmarks}
#'    \item{\code{\link{RunMachineLearningBenchmark}}}{Executes the machine
#'      learning microbenchmarks}
#' }
#'
#' @section Microbenchmark definition classes:
#' \describe{
#'    \item{\code{\link{DenseMatrixMicrobenchmark}}}{Specifies a dense matrix
#'      microbenchmark}
#'    \item{\code{\link{SparseMatrixMicrobenchmark}}}{Specifies a sparse matrix
#'      microbenchmark}
#'    \item{\code{\link{ClusteringMicrobenchmark}}}{Specifies a clustering for
#'      machine learning microbenchmark}
#' }
#' @docType package
#' @name RHPCBenchmark
NULL
