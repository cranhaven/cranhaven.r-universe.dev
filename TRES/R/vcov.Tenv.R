#' @method vcov Tenv
#' @export

vcov.Tenv <- function(object, ...){
  stop("The vcov method is not available for Tenv class. Refer to std_err function if the standard error is desired.")
}
