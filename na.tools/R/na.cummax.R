#' @rdname impute-noncommutative
#' @export

na.cummax <- function( .x, ... ) 
  na.replace(.x, .na=function(x, ...) cummax(x, ... ) )



#' @rdname impute-noncommutative
#' @export

na.cummin <- function( .x, ... ) 
  na.replace(.x, .na=function(x, ...) cummin(x, ... ) )



# #' @rdname impute-noncommutative
# #' @export
# 
# na.cummean <- function( .x, ... ) 
#   na.replace(.x, .na=function(x, ...) cummean(x, ... ) )


#' @rdname impute-noncommutative
#' @export

na.cumsum <- function( .x, ... ) 
  na.replace(.x, .na=function(x, ...) cumsum(x, ... ) )




#' @rdname impute-noncommutative
#' @export

na.cumprod <- function( .x, ... ) 
  na.replace(.x, .na=function(x, ...) cumprod(x, ... ) )
