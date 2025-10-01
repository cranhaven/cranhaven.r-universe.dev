#' Extract coefficients from a classo object
#'
#' @method coef classo
#' @rdname predict.classo
#' @export
#' @export coef.classo
coef.classo <- function(object,s=NULL,exact=FALSE,...){
  predict(object,s=s,type="coefficients",exact=exact,...)
}
