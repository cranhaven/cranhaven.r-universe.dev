
##' Extract the left or right substrings in a character vector.
##' 
##' @title Extract the left or right substrings in a character vector.
##' @aliases right
##' @usage
##' left(string, n)
##' right(string, n)
##' @param string A character vector.
##' @param n How many characters.
##' @return A character vector.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @examples
##' left("hello", 3)

left <- function(string, n) {
	string <- .verifyChar(string)
	OUT <- substr(string, 1, n)
	return(OUT)
}

right <- function(string, n) {
	string <- .verifyChar(string)
	OUT <- substr(string, nchar(string) - n + 1, nchar(string))
	return(OUT)
}



