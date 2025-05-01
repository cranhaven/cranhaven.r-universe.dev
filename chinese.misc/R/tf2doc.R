#' Transform Terms and Frequencies into a Text
#'
#' This function is simply a wrapper of \code{rep}, but allows different structures of input.
#' For rewriting more texts in the same time, see \code{\link{m2doc}}.
#'
#' @param term terms that you want to rewrite into a text. A character vector is preferred, but 
#' matrix, list, data frame are also OK. \code{NA} in the argument will be taken as 
#' letters "NA" and repeated.
#' @param num frequencies of terms in \code{term}. A numeric vector is preferred, but 
#' matrix, list, data frame are also OK. Its length must be equal to that of \code{term}.
#' No \code{NA} is allowed.
#'
#' @return a character vector. Terms are pasted with a space.
#'
#' @export
#' @examples
#' x <- matrix(c("coffee", "milk", "tea", "cola"), nrow = 2)
#' y <- factor(c(5:8))
#' tf2doc(x, y)
tf2doc <-
function(term, num) {
  num <- as.integer(as.numeric2(num))
  if (any(is.na(num))) 
    stop("There should be no NA in num.")
  if (any(num < 0)) 
    stop("Argument num must be integers larger than 0.")
  if (all(num == 0)) {
    message("Every element in num is zero, so the result is a size 0 character!")
    return("")
  } else {
    term <- as.character2(term)
    if (!length(term) == length(num)) 
      stop("The length of term must be equal to that of num.")
    if (any(is.na(term))) 
      warning("NA in argument term is represented by the letters NA.")
    to_return <- paste(rep(term, num), collapse = " ")
	to_return <- whetherencode(to_return)
	return(to_return)
  }
}
