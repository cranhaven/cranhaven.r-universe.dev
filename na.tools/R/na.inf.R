#' @details 
#' 
#' `na.inf` and `na.neginf` replace all missing values with `Inf` and `-Inf` 
#' repectively.  `.
#' 
#' @examples 
#'   na.inf( c( 1, 2, NA, 4) )
#'   na.neginf( c( 1, 2, NA, 4) ) 
#'   
#' @rdname impute-constant
#' @aliases  na.inf
#' @export

na.inf <- function(.x) na.replace(.x, Inf)

#' @rdname impute-constant
#' @aliases na.neginf
#' @export

na.neginf <- function(.x) na.replace(.x, -Inf)
