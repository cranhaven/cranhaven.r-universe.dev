#' @details 
#' `na.mean` replaces `NA` values with the mean of `x`. Internally, 
#' `mean(x, na.rm=TRUE, ... )` is used. If mean cannot be calculated (e.g. x 
#'  isn't numeric) then `x` is returned with a warning.
#' 
#' @rdname impute-commutative
#' @export

na.mean <- function( .x, ... ) 
  na.replace(.x, .na=function(x, ...) mean(x, na.rm=TRUE, ... ) )
