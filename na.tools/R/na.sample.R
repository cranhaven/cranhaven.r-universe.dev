#' na.bootstrap
#' 
#' Replace missing values with value randomly drawn from x
#' 
#' @param .x vector with 
#' @param ... additional arguments passed to [base::sample()]
#' 
#' @details 
#' 
#' `na.random` replaces missing values by sampling the non-missing values. By 
#' default aampling occurs **with replacement** since more valuables may be needed than
#' are available. This function is based on [base::sample()].
#' 
#' The default is to replace bv sampling a population defined by the 
#' non-missing values of `.x` **with replacement** 
#'  
#' `na.random` is an alias for `na.bootstrap`.
#' `
#' @seealso 
#'  * [base::sample()]
#' 
#' 
#' @note `na.bootstrap` is **non-deterministic**. Use 
#'       [base::set.seed()] to make it deterministic
#' 
#' @examples
#'   x <- c(1,NA,3)
#'   na.bootstrap(x)
#'   
#' @export

na.bootstrap <- function(.x, ... ) { 
  # args() <- list( x=.x, replace=TRUE )   # Need this because more values might be needed than available
  # new_args <- modifyList( args, list(...) )
  na.replace( .x, .na=function(x, ...) sample( na.omit(x), replace=TRUE, ...) ) 
}

              
# #' @rdname na.bootstrap
# #' @export
# na.random <- na.bootstrap


#' @rdname na.bootstrap
#' @export
na.resample <- na.bootstrap

# #' @rdname na.bootstrap
# #' @export
# na.sample <- na.bootstrap
