# Introduction
This package performs microbenchmarking for determining the run time
performance of aspects of the R programming environment and packages relevant
to high-performance computation.  The benchmarks are divided into three
categories: dense matrix linear algebra kernels, sparse matrix linear algebra
kernels, and machine learning functionality.  The top-level benchmark functions
covering the three categories are RunDenseMatrixBenchmark,
RunSparseMatrixBenchmark, RunMachineLearningBenchmark.

# Installation
The companion data package [RHPCBenchmarkData](https://github.com/IUResearchAnalytics/RBenchmarking/blob/master/RHPCBenchmarkData_0.1.0.tar.gz)
contains the sparse matrix files needed by the sparse matrix benchmarking
function.

Installation of the benchmarking and companion data packages is trivial with
the use of the `install.packages` function

# Examples
See the vignette named 'vignette' for a more detailed explanation of the package
and additional examples.  New benchmarks can be specified using the classes
`DenseMatrixMicrobenchmark`, `SparseMatrixMicrobenchmark`, and
`ClusteringMicrobenchmark`; see the vignette and the object documentation for
each of these classes to learn how new microbenchmarks can be constructed. 

