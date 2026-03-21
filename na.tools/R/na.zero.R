#' @details 
#' 
#' `na.zero` replaces missing values with `0` which gets coerced to the 
#' `class(x)` as needed. 
#' 
#' @examples 
#'   na.zero( c(1,NA,3) )  # 1 0 3 
#' 
#' @examples 
#'  
#'            
#' @rdname impute-constant
#' @aliases na.zero
#' @export

na.zero <- function(.x) na.replace(.x,0)
