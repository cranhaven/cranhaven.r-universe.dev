
#' @title 
#' Test if NA 
#' 
#' @description 
#' \code{isNA} tests if an object \code{x} is identical to one of \code{NA}, 
#' \code{NA_character_}, \code{NA_complex_}, \code{NA_integer_}, 
#' \code{NA_real_}, or \code{NaN}. 
#' 
#' @param x
#' An R object. 
#' 
#' @return 
#' \code{TRUE} or \code{FALSE}. 
#' 
#' @seealso 
#' \code{\link{isTRUE}}. 
#' 
#' @export
#' 
isNA <- 
function(x)
{
  identical(x, NA) || 
    identical(x, NA_character_) ||
    identical(x, NA_complex_) ||
    identical(x, NA_integer_) ||
    identical(x, NA_real_) ||
    identical(x, NaN)
}
