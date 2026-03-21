#' @details 
#' 
#' `any_na` reports if **any** values are missing. If always returns a logical
#' scalar.
#' 
#' @return
#' 
#' logical scalar; either TRUE or FALSE.
#' 
#' @seealso 
#'  - [base::anyNA()]
#'   
#' @examples
#'   any_na( 1:10 )           # FALSE
#'   any_na( c( 1, NA, 3 ) )  # TRUE
#' 
#' @md
#' @rdname all_na 
#' @export

any_na<- function(x) UseMethod('any_na') 



#' @export
any_na.default <- function(x) anyNA(x)
  

# #' @rdname all.na 
# #' @export
# any_na <- any.na

# #' @rdname all_na
# #' @export 
# na.any <- function(x) {
#   warning("'na.any' is deprecated. Use 'any.na' instead.")
#   any.na(x)
# }
