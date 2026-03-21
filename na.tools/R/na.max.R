#' @details 
#' 
#' `na.max` and `na.min` replace missing values (`NA`) with the maximum or 
#' minimum *of non-missing values `x`.* 
#' ( Internally: `base::max(..., na.rm=TRUE)` and `base::min(..., na.rm=TRUE)`. 
#' `...` has no affect.
#'  
#' @seealso 
#'  * [base::max()] and [base::min()]
#'
#' @rdname impute-commutative
#' @export

na.max <- function( .x, ... ) 
  na.replace(.x, .na=function(x) max(x, na.rm=TRUE ) )


#' @rdname impute-commutative 
#' @export

na.min <- function( .x, ... ) 
  na.replace(.x, .na=function(x) min(x, na.rm=TRUE ) )
