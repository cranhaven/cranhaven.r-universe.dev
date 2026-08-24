#' Convenience function to quickly copy-paste a vector
#'
#' @param x A string with numbers, separated by arbitrary whitespace.
#' @param fn An optional function to apply to the vecor before returning it.
#'
#' @return The numeric vector or result of calling the function
#' @export
#' @rdname qVec
#'
#' @examples qVec('23 	9 	11 	14 	12 	20');
qVec <- function(x, fn=NULL) {
  x <- gsub("[^0-9., ]", "", x);
  x <- gsub("\\s+", " ", x);
  x <- gsub(",", ".", x);
  x <- strsplit(x, " ")[[1]];
  x <- as.numeric(x);
  if (is.null(fn)) {
    return(x);
  } else {
    return(fn(x));
  }
}

#' @rdname qVec
qVecSum <- function(x) {
  return(qVec(x, fn=sum));
}
