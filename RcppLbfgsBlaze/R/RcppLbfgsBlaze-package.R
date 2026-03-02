## Copyright (C) 2024 Ching-Chuan Chen
##
## This file is part of RcppLbfgsBlaze.
##
## RcppLbfgsBlaze is free software: you can redistribute it and/or modify it
## under the terms of the MIT License. You should have received
## a copy of MIT License along with RcppLbfgsBlaze.
## If not, see https://opensource.org/license/mit.

#' RcppLbfgsBlaze - Rcpp interface to the L-BFGS algorithm with Blaze
#'
#' \strong{RcppLbfgsBlaze} constructs a simple interface to the \strong{L-BFGS} algorithm based on \strong{Blaze} for \R and \strong{Rcpp}.
#'
#' This package provides an implementation of the \strong{L-BFGS} algorithm based on \strong{Blaze} for \R and \strong{Rcpp}.
#' The \strong{L-BFGS} algorithm is a popular optimization algorithm for unconstrained optimization problems.
#' \strong{Blaze} is a high-performance \strong{C++} math library for dense and sparse arithmetic.
#' The package provides a simple interface to the \strong{L-BFGS} algorithm and allows users to optimize
#' their objective functions with Blaze vectors and matrices in \R and \strong{Rcpp}.
#'
#' @section Using \strong{RcppLbfgsBlaze}:
#' The simplest way to get started is to create a skeleton of a package
#' using \strong{RcppLbfgsBlaze}.
#'
#' The important steps are
#' \enumerate{
#' \item Include the \samp{RcppBlaze.h} and \samp{lbfgs.h} header files.
#' \item Import \code{Rcpp}. LinkingTo \code{Rcpp}, \code{RcppBlaze} and \code{RcppLbfgsBlaze} by adding these lines to the \samp{DESCRIPTION} file:
#' \preformatted{
#'   Imports: Rcpp (>= 1.0.0)
#'   LinkingTo: Rcpp, RcppBlaze (>= 1.0.0), RcppLbfgsBlaze
#' }
#' \item Link against the \code{BLAS} and \code{LAPACK} libraries, by adding following two lines in the \samp{Makevars} and \samp{Makevars.win} files:
#' \preformatted{
#'   PKG_CXXFLAGS=$(SHLIB_OPENMP_CXXFLAGS)
#'   PKG_LIBS = $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS) $(SHLIB_OPENMP_CXXFLAGS)
#' }
#' }
#'
#' @author
#' For RcppLbfgsBlaze: Ching-Chuan Chen
#' Maintainer: Ching-Chuan Chen <zw12356@gmail.com>
#'
#' @references
#' \enumerate{
#' \item Blaze project: \url{https://bitbucket.org/blaze-lib/blaze}.
#' \item LBFGS-blaze: \url{https://github.com/ChingChuan-Chen/LBFGS-blaze}
#' \item LBFGS-Lite: \url{https://github.com/ZJU-FAST-Lab/LBFGS-Lite}
#' \item liblbfgs: \url{https://github.com/chokkan/liblbfgs}
#' }
#'
#' @keywords package interface
#' @name RcppLbfgsBlaze-package
#' @useDynLib RcppLbfgsBlaze, .registration = TRUE
"_PACKAGE"
