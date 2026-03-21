#' @details 
#'
#' `na.constant` replaces missing values with a scalar constant. It is a wrapper
#' around [na.replace()] but permits `.na` to only be a scalar.
#'
#' @examples 
#'   na.constant( c(1,NA,2), -1 )
#'  
#' @rdname impute-constant
#' @export

na.constant<- function(.x, .na) {
  
  if( ! is.atomic(.na) || length(.na) != 1 )
    stop( "'na.constant' works with scalar values only.") 
  
  na.replace(.x, .na)
  
}
