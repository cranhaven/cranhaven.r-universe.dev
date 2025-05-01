#' Rewrite Terms and Frequencies into Many Files
#'
#' Given a matrix representing a document term matrix, this function takes each row as term 
#' frequencies for one file, and rewrite each row as a text.
#' Some text mining tools other than R accept
#' segmented Chinese texts.
#' If you already convert texts into a matrix, you can use this function to convert 
#' it into texts, corpus
#' or create document term matrix again.
#'
#' @param m a numeric matrix, data frame is not allowed. It must represent a document term 
#' matrix, rather than a term document matrix. Each row of the matrix represents a text. The 
#' matrix should have column names as terms to be written, but if it is \code{NULL}, the 
#' function will take them as "term1", "term2", "term3", ...No \code{NA} in the matrix 
#' is allowed.
#' @param checks should be \code{TRUE} or \code{FALSE}. If it is TRUE, the function will 
#' check whether there is any \code{NA} in the input, whether it is numeric,  and whether 
#' there is any negative number. Default is \code{FALSE} to save time.
#'
#' @return a character vector, each element is a text with repeated 
#' terms (by \code{\link{rep}}) linked by a space.
#'
#' @export
#' @examples
#' s <- sample(1:5, 20, replace = TRUE)
#' m <- matrix(s, nrow = 5)
#' colnames(m) <- c("r", "text", "mining", "data")
#' m2doc(m)
m2doc <-
function(m, checks = FALSE) {
  if (!is.matrix(m)) {
    stop("Argument m should be a matrix.")
  }
  coln <- colnames(m)
  if (is.null(coln)) {
    coln <- paste("term", 1:ncol(m), sep = "")
  }
  if (checks == TRUE) {
    if (any(is.na(m))) 
      stop("No NA allowed.")
    if (!is.numeric(m[1, 1])) 
      stop("Cells of m must all be numeric.")
    if (any(m < 0)) 
      stop("Numbers in m must be positive.")
  }
  innertf2doc <- function(num, term) {
    if (all(num == 0)) {
      message("Some rows have nothing, return character with nchar equal to 0.")
      return("")
    }
    else {
      return(paste(rep(term, num), collapse = " "))
    }
  }
  many_row <- apply(m, 1, innertf2doc, term = coln)
  many_row <- whetherencode(many_row)
  return(many_row)
}
