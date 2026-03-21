#' @name impute-noncommutative
#' @title non-commutative imputation
#'
#' Impute missing values using non-commutative functions, i.e. where the order 
#' **matters**.
#' 
#' @param .x atomic-vector with 0 or more missing values
#' @param ... additional arguments 
#' 
#' @details
#' 
#' Non-commutative imputations functions assume that `.x` is in the proper order
#' since the values depend on order. Usually, this is relevant then `.x` is part
#' of a table.
#'   
#'  These functions replaces `NA` values with the cummulative max of `.x`. Internally, 
#' `fun(.x, na.rm=TRUE, ... )` is used. If the function cannot be calculated 
#' (e.g. `.x`  isn't numeric) then `x` is returned unchanged with a warning.
#' 
#' Use of `na.cumsum` and `na.cumprod` are dangerous since they omit missing 
#' values that may contribute to 
#' @seealso 
#'  * [base::cummax()]
#'  * [impute-commutative]
#'   
#' @rdname impute-noncommutative

NULL
