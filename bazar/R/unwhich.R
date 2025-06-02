#' @title 
#' Quasi-inverse of the 'which' function
#' 
#' @description 
#' The \code{unwhich} function is a kind of inverse 
#' of the \code{\link{which}} function. 
#' 
#' @param w
#' A vector of integers; morally the result of a call to \code{which}. 
#' 
#' @param n
#' integer. The length of the result; morally the length of the 
#' \code{x} argument of a call to \code{which}. 
#' 
#' @return 
#' A logical vector of length \code{n}. 
#' 
#' @seealso \code{\link{which}}. 
#' 
#' @export
#' 
#' @examples 
#' x1 <- c(TRUE, FALSE, TRUE, TRUE)
#' x2 <- unwhich(which(x1), length(x1))
#' identical(x1, x2) # TRUE
#' 
#' w1 <- c(2, 4, 5, 1, 1)
#' w2 <- which(unwhich(w1, 10))
#' identical(sort(unique(as.integer(w1))), w2) # TRUE
#' 
unwhich <- 
function(w, 
         n)
{
  seq_len(n) %in% w
}
