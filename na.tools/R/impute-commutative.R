#' @name impute-commutative
#' @title Imputation by Cummutative Functions
#' 
#' Impute using replacement values calculated from a univariate, cummuative 
#' function.
#' 
#' @param .x vector in which `NA` values are to be replaced. The ordering of `x`
#'        does not matter. 
#' @param ... additional arguments passed to lower-level summary functions.
#' 
#' @details 
#' 
#' This collection of functions calculates a replacement value using an 
#' unvariate function where the order of values in `x` do not matter, 
#' i.e. commutative.
#' 
#' 
#' @return 
#' 
#' A vector of `class(x)` and `length(x)` in which missing values (NA) have 
#' been replaced the result of a function call: \deqn{ fun(x, ...) }
#' 
#' @seealso 
#'  * [na.replace()] - used internally by these functions
#'  * [na.constant()]
#'  
#' @rdname impute-commutative

NULL 
