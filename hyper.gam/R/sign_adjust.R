

#' @title Sign Adjustment
#' 
#' @param object ..
#' 
#' @param ... parameters of function [cor_xy.hyper_gam()]
#' 
#' @keywords internal
#' @name sign_adjust
#' @export
sign_adjust <- function(object, ...) UseMethod(generic = 'sign_adjust')


#' @rdname sign_adjust
#' @returns 
#' The `S3` method [sign_adjust.hyper_gam()] returns a \link[base]{numeric} \link[base]{vector}.
#' @export sign_adjust.hyper_gam
#' @export
sign_adjust.hyper_gam <- function(object, ...) {
  
  sgn <- object |>
    cor_xy.hyper_gam(...) |>
    sign()
  
  return(sgn * object$linear.predictors)
  
}



