#' @title Validation and normalisation for correlation
#'
#' @description
#' Validates and normalises input for correlation computations. Accepts either a
#' numeric matrix or a data frame, filters numeric columns, checks dimensions and
#' (optionally) missing values, and returns a numeric (double) matrix with
#' preserved column names.
#'
#' @details
#' Rules enforced:
#' \itemize{
#'   \item Input must be a matrix or data.frame.
#'   \item Only numeric (integer or double) columns are retained (data.frame path).
#'   \item At least two numeric columns are required.
#'   \item All columns must have the same length and \eqn{\ge} 2 observations.
#'   \item Missing values are not allowed when \code{check_na = TRUE}.
#'   \item Returns a \code{double} matrix; integer input is converted once.
#' }
#'
#' @param data A matrix or data frame. Non-numeric columns are dropped
#'   (data.frame path). For matrix input, storage mode must be integer or double.
#' @param check_na Logical (default \code{TRUE}). If \code{TRUE}, validate and
#'   reject inputs containing \code{NA}/\code{NaN}/\code{Inf}. Set to
#'   \code{FALSE} when an upstream routine (e.g., pairwise-complete kernels)
#'   will handle missingness per pair.
#'
#' @return A numeric matrix (type \code{double}) with column names preserved.
#'
#' @seealso [pearson_corr()], [spearman_rho()], [kendall_tau()]
#'
#' @author Thiago de Paula Oliveira
#' @keywords internal
#' @name matrixCorr-internal
validate_corr_input <- function(data, check_na = TRUE) {
  if (!is.logical(check_na) || length(check_na) != 1L)
    stop("`check_na` must be a single logical.", call. = FALSE)
  validate_corr_input_cpp(data, check_na)
}

