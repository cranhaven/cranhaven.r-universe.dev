#' `na.median` imputes with the median value of `x`. The median is only valid
#' for numeric or logical values.
#' 
#' @seealso 
#'  [median()]
#'   
#' @examples 
#'   na.median( c(1,2,NA_real_,3) )
#'       
#' @rdname impute-commutative
#' @import stats 
#' @export

na.median <- function( .x, ... ) 
  na.replace( .x, .na=function(x, ...) median(x, na.rm=TRUE, ... ) )


#' @details 
#' 
#' `na.quantile` imputes with a quantile. The quantile is specified by a 
#' `probs` argument that is passed to [stats::quantile()]. If `probs` can be 
#' a scalar value in which all values are replaced by that quantile or a vector 
#' of `length(.x)` values which replaces the missing values of x with the 
#' `probs`. The ability to provide a vector may be deprecated in the future.
#'   
#' @seealso
#'   * [quantile()]
#'
#' @examples
#'   na.quantile( c(1,2,NA_real_,3), prob=0.4 )
#' 
#' @rdname impute-commutative
#' @export

na.quantile <- function(.x, ... ) 
  na.replace( .x, .na=quantile(.x, na.rm=TRUE, ... ) )
