.onLoad <- function(lib, pkg) {
  loadNamespace("gmp")    # to use as.character.bigq
  loadNamespace("Matrix") # to use as.matrix
  # Work around bug in code checking in R 4.2.2 for use of packages
  dummy_gmp <- function(x) gmp::as.bigq(x)
  dummy_matrix <- function(x) Matrix::as.matrix(x)
}

#' @title Pipe operator
#' @description This is the 'magrittr' pipe operator. We import it in this
#' package in order to help the user to construct the \code{model} argument of
#' the \code{\link{getAb}} function.
#'
#' @importFrom magrittr %>%
#' @export %>%
#' @docType import
#' @name pipe-operator
#' @aliases %>%
NULL
