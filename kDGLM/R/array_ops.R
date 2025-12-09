# ### Array ops ####
#
# # Only execute once to prepare the cpp code.
#
# library(einsum)
# func_list=list(
#   array_mult_left=einsum_generator('ixk,xj -> ijk',FALSE),
#   array_mult_right=einsum_generator('xik,jx -> jik',FALSE),
#   array_transp=einsum_generator('ijk -> jik',FALSE),
#   array_collapse_left=einsum_generator('ixk,x -> ik',FALSE),
#   array_collapse_right=einsum_generator('xik,x -> ik',FALSE)
# )
#
# for(f_name in names(func_list)){
#   write(paste('#include <Rcpp.h>',
#               'using namespace Rcpp;',
#               '',
#               '// [[Rcpp::export]]',
#               stringr::str_replace_all(func_list[[f_name]],
#                                        'einsum_impl_func',
#                                        f_name),
#               sep='\n'),
#         paste0('src/',f_name,'.cpp'))
# }


# #' @useDynLib kDGLM, .registration=TRUE
# #' @importFrom Rcpp sourceCpp
# NULL


#' array_mult_left
#'
#' Calculates the matrix product between an array and a matrix.
#'
#' For an array A with shapes n x m x k and a matrix B with shape m x l, this operations returns an array C, with shapes n x l x k, so that C[,,i] = A[,,i] %*% B.
#'
#' @param A A 3-D array with shapes n x m x k.
#' @param B A matrix with shapes m x l.
#'
#' @keywords internal
array_mult_left <- function(A, B) {
  apply(A, 3, function(x) {
    x %*% B
  }, simplify = FALSE) |> simplify2array(except = NULL)
}

#' array_mult_right
#'
#' Calculates the matrix product between an array and a matrix.
#'
#' For an array A with shapes m x n x k and a matrix B with shape l x m, this operations returns an array C, with shapes l x n x k, so that C[,,i] = B %*% A[,,i].
#'
#' @param A A 3-D array with shapes n x m x k.
#' @param B A matrix with shapes l x n.
#'
#' @keywords internal
array_mult_right <- function(A, B) {
  apply(A, 3, function(x) {
    B %*% x
  }, simplify = FALSE) |> simplify2array(except = NULL)
}

#' array_transp
#'
#' Calculates the element-wise transposition of an array.
#'
#' For an array A with shapes n x m x k, this operations returns an array C, with shapes m x n x k, so that C[,,i] = t(A[,,i]).
#'
#' @param A A 3-D array.
#'
#' @keywords internal
array_transp <- function(A) {
  apply(A, 3, function(x) {
    t(x)
  }, simplify = FALSE) |> simplify2array(except = NULL)
}

#' array_collapse_left
#'
#' Calculates the matrix product between an array and a vector.
#'
#' For an array A with shapes n x m x k and a vector B with shape m, this operations returns a matrix C, with shapes n x k, so that C[,i] = A[,,i] %*% B.
#'
#' @param A A 3-D array with shapes n x m x k.
#' @param B A matrix with shapes m x 1.
#'
#' @keywords internal
array_collapse_left <- function(A, B) {
  array_mult_left(A, B)[, 1, ]
}

#' array_collapse_right
#'
#' Calculates the matrix product between an array and a vector.
#'
#' For an array A with shapes m x n x k and a vector B with shape m, this operations returns a matrix C, with shapes n x k, so that C[,i] = B %*% A[,,i].
#'
#' @param A A 3-D array with shapes n x m x k.
#' @param B A matrix with shapes 1 x n.
#'
#' @keywords internal
array_collapse_right <- function(A, B) {
  array_mult_right(A, B)[1, , ]
}
