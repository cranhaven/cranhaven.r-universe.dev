#' Compare two characters from left
#' Much useful for arguments input. Case is ignored.
#' @param x1 one character
#' @param x2 the other character
#'
#' @return logical
#' @export
#'
#' @examples
#' left_equal('o','OK')
#' left_equal('ok','O')
#' left_equal('ok','Ok')
left_equal <- function(x1,x2){
    x <- c(x1,x2)[which.min(c(nchar(x1),nchar(x2)))]
    object <- c(x1,x2)[which.max(c(nchar(x1),nchar(x2)))]
    tolower(x) == tolower(left(object,nchar(x)))
}

#' Compare two characters from right
#' Much useful for arguments input. Case is ignored.
#' @param x1 one character
#' @param x2 the other character
#'
#' @return logical
#' @export
#'
#' @examples
#' right_equal('k','OK')
#' right_equal('ok','k')
#' right_equal('ok','Ok')
right_equal <- function(x1,x2){
    x <- c(x1,x2)[which.min(c(nchar(x1),nchar(x2)))]
    object <- c(x1,x2)[which.max(c(nchar(x1),nchar(x2)))]
    tolower(x) == tolower(right(object,nchar(x)))
}