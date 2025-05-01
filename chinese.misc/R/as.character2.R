#' An Enhanced Version of as.character
#'
#' This function manages to coerce one or more objects into a character vector. Unlike 
#' \code{as.character}, this function can handle data frames, lists and recursive lists 
#' (lists of lists), even when there are factor objects inside data frames and lists. If there is any 
#' \code{NULL} object in a list, \code{as.character2} will coerce that element into 
#' \code{character(0)} rather than the character "NULL", which is what 
#' \code{as.character} does. When the object is of class matrix or data frame, the function 
#' will open it by column. The order of characters in result manages to keep accordance 
#' with that of the input object. 
#'
#' @param ... one or more objects to be coerced.
#'
#' @return a character vector
#'
#' @export
#' @import purrr
#' @examples
#' as.character2(NULL, NULL)
#' # Try a list of NULLs
#' null_list <- list(a = NULL, b = NULL, c = NULL)
#' # Compare the different results of as.character 
#' # and as.character2. In fact, we usually 
#' # want the latter one.
#' as.character(null_list)
#' as.character2(null_list)
#' # Try a list with a data frame in it
#' df <- data.frame(matrix(c(66,77,NA,99), nrow = 2))
#' l <- list(a = 1:4, b = factor(c(10,20,NA, 30)), c = c('x', 'y', NA, 'z'), d = df)
#' as.character2(l)
#' # Try a list of lists
#' l2 <- list(l, l, cha = c('a', 'b', 'c'))
#' as.character2(l2)
as.character2 <- function(...) {
  X <- list(...)
  lengthX <- length(X)  
  if (lengthX == 0){
	FINAL <- character(0)
  } else {
    FINAL <- inner_1_level(X)
  }	
  if (length(FINAL) == 0){
	FINAL <- character(0)
  } else {
	FINAL <- unlist(lapply(FINAL, do_final_flatten))
	names(FINAL)=NULL
  }
  if (length(FINAL) == 0) FINAL=character(0)
  FINAL
}	

inner_1_level=function(x){
	y <- purrr::flatten(x)
	if (length(y) == 0){
		return(character(0))
	} else {
		if (sum(sapply(y, FUN = function(xxx) class(xxx)[1] == "list")) > 0)	Recall(y) else y
	}
}

do_final_flatten <- function(obj){
  if (length(obj) == 0) {
	return(character(0))
  } else if (class(obj)[1] == "data.frame") {
	return(as.vector(apply(obj, 2, as.character)))
  } else if (class(obj)[1] == "data.table"){
	return(as.character(as.matrix(obj))) # must first as.matrix !
  } else if (class(obj)[1] == "SimpleCorpus") {
    obj$meta$language <- NULL
	return(obj$content)
  }	else {
	return(as.character(obj))
  }
}
	